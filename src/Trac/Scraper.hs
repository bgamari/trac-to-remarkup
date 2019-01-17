{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE OverloadedStrings #-}
module Trac.Scraper
where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Text.Taggy
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative
import qualified Trac.Parser as R
import Control.Exception
import Trac.Db.Types (CommentRef (..))
import Trac.Convert (convertBlocks, LookupComment)
import Trac.Writer (writeRemarkup)
import Data.Maybe
import Data.Char
import Text.Read (readMaybe)
import Data.List

httpGet :: String -> IO LBS.ByteString
httpGet url = do
  manager <- newTlsManager
  rq <- parseUrlThrow url
  rp <- httpLbs rq manager
  return $ responseBody rp

data ConversionError = ConversionError String
  deriving (Show)

instance Exception ConversionError where
  displayException (ConversionError err) = "Conversion error: " ++ err

convert :: String -> String -> String -> Maybe Int -> Maybe String -> LookupComment -> LBS.ByteString -> IO String
convert base org proj mn msrcname cm s = do
  let blocks =
        nodeToBlocks $
        maybe (throw $ ConversionError "Main content not found") id $
        extractPayload $
        parseHtml s

  fmap (writeRemarkup base org proj) $ convertBlocks mn cm blocks

parseHtml :: LBS.ByteString -> [Node]
parseHtml =
  parseDOM True . LText.pack . LUTF8.toString

extractPayload :: [Node] -> Maybe Node
extractPayload [] =
  Nothing
extractPayload (x:xs) = 
  extractPayloadFrom x <|> extractPayload xs

attrIs :: HashMap.HashMap Text Text -> Text -> Text -> Bool
attrIs eltAttrs key val
  | HashMap.lookup key eltAttrs == Just val
  = True
  | otherwise
  = False

lookupAttr :: Text -> Node -> Maybe Text
lookupAttr key (NodeElement (Element {..}))
  = HashMap.lookup key eltAttrs
lookupAttr _ _
  = Nothing

extractPayloadFrom :: Node -> Maybe Node
extractPayloadFrom (NodeContent _) = Nothing
extractPayloadFrom node@(NodeElement (Element {..}))
  | eltName == "div" && HashMap.lookup "id" eltAttrs == Just "wikipage"
  = Just node
  | otherwise
  = extractPayload eltChildren

-- We use a custom folding strategy here in order to deal with inline-level
-- elements in a context where block-level elements are expected. When
-- that happens, we want to group consecutive inline elements and wrap
-- each group in a single Para, but we need to keep existing block-level
-- elements intact.
nodesToBlocks :: [Node] -> [R.Block]
nodesToBlocks nodes =
  go $ zip (map nodeToInlines nodes) (map nodeToBlocks nodes)
  where
    -- This case deals with successful block-level parses: this means
    -- that the node results in one or more proper Blocks, and we can
    -- just use those.
    go ((_, blocks):xs)
      | not (null blocks)
      = blocks ++ go xs
    -- Base case. The usual.
    go [] = []
    -- This is what happens when we have no successful block-level parse:
    -- we grab all consecutive non-parses from the start of the list,
    -- wrap them in a Para, and recurse into the rest of the list.
    go xs
      = let (is, rem) = break (not . null . snd) xs
        in (R.Para $ concatMap fst is) :
           go rem

childrenToBlocks :: Node -> [R.Block]
childrenToBlocks (NodeElement Element{..})
  = nodesToBlocks eltChildren
childrenToBlocks _ = []

-- | Generate 'Blocks' for a 'Node'. Non-block nodes, including text content,
-- yield an empty list.
nodeToBlocks :: Node -> [R.Block]
nodeToBlocks (NodeContent str) = []
nodeToBlocks (NodeElement elem) = elemToBlocks elem

-- | Generate 'Blocks' for an 'Element'. Non-block elements yield an
-- empty list.
elemToBlocks :: Element -> [R.Block]
elemToBlocks Element {..}
  ----- lists -----
  | eltName == "ul"
  = [R.List R.BulletListType $ map nodeToLi eltChildren]
  | eltName == "ol"

  ----- code blocks -----
  = [R.List R.NumberedListType $ map nodeToLi eltChildren]
  | eltName == "pre"
  = let syntaxMay = Text.unpack <$> HashMap.lookup "class" eltAttrs
    in [R.Code syntaxMay $ Text.unpack . mconcat . map textContent $ eltChildren]

  ----- paragraphs -----
  | eltName == "p"
  = [R.Para $ nodesToInlines eltChildren]

  ----- divider -----
  | eltName == "hr"
  = [R.HorizontalLine]

  ----- headings -----
  | eltName == "h1"
  = [R.Header 1 (nodesToInlines eltChildren) []]
  | eltName == "h2"
  = [R.Header 2 (nodesToInlines eltChildren) []]
  | eltName == "h3"
  = [R.Header 3 (nodesToInlines eltChildren) []]
  | eltName == "h4"
  = [R.Header 4 (nodesToInlines eltChildren) []]
  | eltName == "h5"
  = [R.Header 5 (nodesToInlines eltChildren) []]
  | eltName == "h6"
  = [R.Header 6 (nodesToInlines eltChildren) []]
  | eltName == "blockquote"
  = case HashMap.lookup "class" eltAttrs of
      Just "citation" ->
        [R.Discussion $ nodesToBlocks eltChildren]
      _ ->
        [R.BlockQuote $ nodesToInlines eltChildren]

  ----- tables -----
  | eltName == "table"
  = nodesToTable eltChildren

  ----- definition lists -----
  | eltName == "dl"
  = nodesToDL eltChildren

  ----- page outline (skip) -----
  -- We use an empty Para, because returning an empty list would trigger
  -- the "grab nested inlines and wrap in a Para" fallback in nodesToInlines.
  | eltName == "div"
  , attrIs eltAttrs "class" "wiki-toc"
  = [R.Para []]

  ----- generic block-level elements
  | eltName `elem` ["div", "section"]
  = nodesToBlocks eltChildren

  | otherwise
  = []

nodesToDL :: [Node] -> [R.Block]
nodesToDL = (:[]) . R.DefnList . nodesToDLEntries

nodesToDLEntries :: [Node] -> [(R.Blocks, [R.Blocks])]
nodesToDLEntries nodes =
  go nodes
  where
    go :: [Node] -> [(R.Blocks, [R.Blocks])]
    go []
      = []
    go (NodeContent str : xs)
      = throw $ ConversionError "Expected dt, found TEXT"
    go (headNode@(NodeElement Element{..}) : xs)
      | eltName == "dt"
      = let (ddNodes, rem) = break (not . isDD) xs
            dds = map childrenToBlocks ddNodes
            dt = childrenToBlocks headNode
        in ((dt, dds) : go rem)

isDD :: Node -> Bool
isDD (NodeElement Element{..})
  | eltName == "dd"
  = True
isDD _
  = False

isWhitespace :: Node -> Bool
isWhitespace (NodeContent str)
  | Text.null str
  = True
  | Text.all isSpace str
  = True
isWhitespace _
  = False

nodesToTable :: [Node] -> [R.Block]
nodesToTable = (:[]) . R.Table . nodesToTableRows

nodesToTableRows :: [Node] -> [R.TableRow]
nodesToTableRows (NodeElement (Element "thead" _ xs) : ns) =
  -- the Parser AST doesn't allow us to distinguish thead from tbody,
  -- so we'll just throw everything in one big table.
  nodesToTableRows xs ++ nodesToTableRows ns
nodesToTableRows (NodeElement (Element "tbody" _ xs) : ns) =
  nodesToTableRows xs ++ nodesToTableRows ns
nodesToTableRows [] =
  []
nodesToTableRows (x:xs)
  | isWhitespace x
  = nodesToTableRows xs
  | otherwise
  = nodeToTR x : nodesToTableRows xs

nodeToTR :: Node -> R.TableRow
nodeToTR (NodeContent str) =
  -- Nothing useful we can do here other than dump the string in a
  -- single-cell table row
  [R.TableCell [R.Para [R.Str $ Text.unpack str]]]
nodeToTR (NodeElement Element {..})
  | eltName == "tr"
  = map nodeToTableCell eltChildren
  | otherwise
  = throw $ ConversionError $ "Expected <tr>, but found <" ++ Text.unpack eltName ++ ">"

nodeToTableCell :: Node -> R.TableCell
nodeToTableCell (NodeElement Element {..})
  | eltName == "th"
  = R.TableHeaderCell $ nodesToBlocks eltChildren
  | eltName == "td"
  = R.TableHeaderCell $ nodesToBlocks eltChildren
  | otherwise
  = throw $ ConversionError $ "Expected <th> or <td>, but found <" ++ Text.unpack eltName ++ ">"
nodeToTableCell (NodeContent str)
  = R.TableCell [R.Para [R.Str $ Text.unpack str]]

nodeToLi :: Node -> R.Block
nodeToLi node = R.Para $ nodeToInlines node

nodesToInlines :: [Node] -> [R.Inline]
nodesToInlines = concatMap nodeToInlines

nodeToInlines :: Node -> [R.Inline]
nodeToInlines node@(NodeElement (Element {..}))
  ------------- common markup -------------
  | eltName == "b" || eltName == "strong"
  = [R.Bold $ nodesToInlines eltChildren]
  | eltName == "i" || eltName == "em"
  = [R.Italic $ nodesToInlines eltChildren]
  | eltName == "tt" || eltName == "code"
  = [R.Monospaced Nothing . Text.unpack $ textContent node]

  ------------- inline media -------------
  | eltName == "img"
  = [R.Image]

  ------------- line break -------------
  | eltName == "br"
  = [R.LineBreak]

  ------------- cruft -------------
  -- Skip @<span class="icon"></span>@: these are icons on external links,
  -- injected by Trac itself; they'll only get in the way, so we'll skip them.
  | eltName == "span"
  , attrIs eltAttrs "class" "icon"
  = []

  ------------- links -------------
  -- * ticket query links (query=...)
  | eltName == "a"
  , Just issueQuery <- takeIssueQuery =<< lookupAttr "href" node
  = (nodesToInlines eltChildren)
    ++
    [ R.Space, R.Str "(Ticket query:", R.Space ]
    ++ 
    ( intercalate
        [ R.Str ",", R.Space ]
        [ [ R.Str (key ++ ": " ++ val) ]
        | (key, val) <- issueQuery
        ]
    )
    ++
    [ R.Str ")" ]

  -- * wiki links
  | eltName == "a"
  , Just wikiname <- takeWikiName =<< lookupAttr "href" node
  , attrIs eltAttrs "class" "wiki"
  = [R.WikiLink wikiname (Just . map (Text.unpack . textContent) $ eltChildren)]

  -- * ticket links
  | eltName == "a"
  , Just ticketNumber <- takeTicketNumber =<< lookupAttr "href" node
  = [R.TracTicketLink ticketNumber (Just . map (Text.unpack . textContent) $ eltChildren)]

  -- TODO:
  -- * ticket comment links
  -- * anchors (<a> without href)
  | eltName == "a"
  , Just url <- lookupAttr "href" node
  = [R.Link (Text.unpack url) (map (Text.unpack . textContent) $ eltChildren)]

  ------------- text content -------------
  | otherwise
  = nodesToInlines eltChildren
nodeToInlines n = textToInlines $ textContent n

takeWikiName :: Text -> Maybe String
takeWikiName = fmap Text.unpack . Text.stripPrefix "/trac/ghc/wiki/"

takeTicketNumber :: Text -> Maybe Int
takeTicketNumber url =
  readMaybe =<<
  Just . Text.unpack . Text.takeWhile isDigit =<<
  Text.stripPrefix "/trac/ghc/ticket/" url

takeIssueQuery :: Text -> Maybe [(String, String)]
takeIssueQuery url = do
  tail <- Text.stripPrefix "/trac/ghc/query?" url
  Just . map splitPair . Text.splitOn "&" $ tail
  where
    splitPair str =
      let (l, r) = Text.breakOn "=" str
      in (Text.unpack l, Text.unpack $ Text.drop 1 r)

textToInlines :: Text -> [R.Inline]
textToInlines = (:[]) . R.Str . Text.unpack

textContent :: Node -> Text
textContent (NodeContent str) =
  Text.replace "\x200b" "" str
textContent (NodeElement (Element {..})) =
  mconcat . map textContent $ eltChildren
