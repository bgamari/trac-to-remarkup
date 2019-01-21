{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE OverloadedStrings #-}
module Trac.Scraper
where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
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
import Logging
import Text.Printf (printf)
import Network.HTTP.Types.Status (Status (..))
import Control.Monad.Reader

httpGet :: Logger -> String -> IO LBS.ByteString
httpGet logger url = withContext logger url $ do
  manager <- newTlsManager
  writeLog logger "HTTP" $ "GET " ++ url
  rq <- parseUrlThrow url
  rp <- httpLbs rq manager
  writeLog logger "HTTP" $
    printf
      "%03i %s"
      (statusCode $ responseStatus rp)
      (UTF8.toString . statusMessage $ responseStatus rp)
  return $ responseBody rp

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
a <++> b = (++) <$> a <*> b
infixr 5 <++>

data ConversionError = ConversionError String
  deriving (Show)

instance Exception ConversionError where
  displayException (ConversionError err) = "Conversion error: " ++ err

type Convert = ReaderT Logger IO

runConvert :: Logger -> Convert a -> IO a
runConvert logger action =
  runReaderT action logger

convert :: Logger -> String -> String -> String -> Maybe Int -> Maybe String -> LookupComment -> LBS.ByteString -> IO String
convert logger base org proj mn msrcname cm s =
  withContext logger "convert" $ do
    blocks <- runConvert logger $
          nodeToBlocks $
          maybe (throw $ ConversionError "Main content not found") id $
          extractPayload $
          parseHtml s

    fmap (writeRemarkup base org proj) $
      convertBlocks mn cm blocks

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

attrContains :: HashMap.HashMap Text Text -> Text -> Text -> Bool
attrContains eltAttrs key val
  | Just aval <- HashMap.lookup key eltAttrs
  , val `elem` Text.words aval
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
nodesToBlocks :: [Node] -> Convert [R.Block]
nodesToBlocks nodes = do
  (is, bs) <- go nodes -- =<< (zip <$> mapM nodeToInlines nodes <*> mapM nodeToBlocks nodes)
  if null is then
    pure bs
  else
    pure (R.Para is : bs)
  where
    go :: [Node] -> Convert ([R.Inline], [R.Block])
    go [] = pure ([], [])
    go (n:ns) = do
      b <- nodeToBlocks n
      (is, bs) <- go ns
      if null b then do
        -- Block-level parse unsuccessful, interpret as inlines instead
        i <- nodeToInlines n
        pure (i ++ is, bs)
      else
        pure ([], b ++ [R.Para is] ++ bs)

childrenToBlocks :: Node -> Convert [R.Block]
childrenToBlocks node@(NodeElement Element{..})
  = withContextM (dumpNode node) (nodesToBlocks eltChildren)
childrenToBlocks _
  = pure []

-- | Generate 'Blocks' for a 'Node'. Non-block nodes, including text content,
-- yield an empty list.
nodeToBlocks :: Node -> Convert [R.Block]
nodeToBlocks (NodeContent str) =
  pure []
nodeToBlocks node@(NodeElement elem) =
  withContextM (dumpNode node) (elemToBlocks elem)

one :: a -> [a]
one = (:[])

-- | Generate 'Blocks' for an 'Element'. Non-block elements yield an
-- empty list.
elemToBlocks :: Element -> Convert [R.Block]
elemToBlocks Element {..}
  ----- lists -----
  | eltName == "ul"
  = one . R.List R.BulletListType <$> mapM childrenToBlocks eltChildren
  | eltName == "ol"
  = one . R.List R.NumberedListType <$> mapM childrenToBlocks eltChildren

  ----- code blocks -----
  | eltName == "pre"
  = let syntaxMay = Text.unpack <$> HashMap.lookup "class" eltAttrs
    in pure . one . R.Code syntaxMay . Text.unpack . mconcat . map textContent $ eltChildren

  ----- paragraphs -----
  | eltName == "p"
  = one . R.Para <$> nodesToInlines eltChildren

  ----- divider -----
  | eltName == "hr"
  = pure . one $ R.HorizontalLine

  ----- headings -----
  | eltName == "h1"
  = fmap one $ R.Header 1 <$> nodesToInlines eltChildren <*> pure []
  | eltName == "h2"
  = fmap one $ R.Header 2 <$> nodesToInlines eltChildren <*> pure []
  | eltName == "h3"
  = fmap one $ R.Header 3 <$> nodesToInlines eltChildren <*> pure []
  | eltName == "h4"
  = fmap one $ R.Header 4 <$> nodesToInlines eltChildren <*> pure []
  | eltName == "h5"
  = fmap one $ R.Header 5 <$> nodesToInlines eltChildren <*> pure []
  | eltName == "h6"
  = fmap one $ R.Header 6 <$> nodesToInlines eltChildren <*> pure []
  | eltName == "blockquote"
  = one . R.Discussion <$> nodesToBlocks eltChildren

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
  = pure . one $ R.Para []

  ----- generic block-level elements
  | eltName `elem` ["div", "section"]
  = nodesToBlocks eltChildren

  | otherwise
  = pure []

nodesToDL :: [Node] -> Convert [R.Block]
nodesToDL = fmap (one . R.DefnList) . nodesToDLEntries

nodesToDLEntries :: [Node] -> Convert [(R.Blocks, [R.Blocks])]
nodesToDLEntries nodes =
  go nodes
  where
    go :: [Node] -> Convert [(R.Blocks, [R.Blocks])]
    go []
      = pure []
    go (NodeContent str : xs)
      = throw $ ConversionError "Expected dt, found TEXT"
    go (headNode@(NodeElement Element{..}) : xs)
      | eltName == "dt"
      = do
          let (ddNodes, rem) = break (not . isDD) xs
          dds <- mapM childrenToBlocks ddNodes
          dt <- childrenToBlocks headNode
          ((dt, dds) :) <$> go rem

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

nodesToTable :: [Node] -> Convert [R.Block]
nodesToTable = fmap (one . R.Table) . nodesToTableRows

nodesToTableRows :: [Node] -> Convert [R.TableRow]
nodesToTableRows (NodeElement (Element "thead" _ xs) : ns) =
  -- the Parser AST doesn't allow us to distinguish thead from tbody,
  -- so we'll just throw everything in one big table.
  (++) <$> nodesToTableRows xs <*> nodesToTableRows ns
nodesToTableRows (NodeElement (Element "tbody" _ xs) : ns) =
  (++) <$> nodesToTableRows xs <*> nodesToTableRows ns
nodesToTableRows [] =
  pure []
nodesToTableRows (x:xs)
  | isWhitespace x
  = nodesToTableRows xs
  | otherwise
  = (:) <$> nodeToTR x <*> nodesToTableRows xs

nodeToTR :: Node -> Convert R.TableRow
nodeToTR (NodeContent str) =
  -- Nothing useful we can do here other than dump the string in a
  -- single-cell table row
  pure . one . R.TableCell . one . R.Para . one . R.Str $ Text.unpack str
nodeToTR (NodeElement Element {..})
  | eltName == "tr"
  = mapM nodeToTableCell eltChildren
  | otherwise
  = throw $ ConversionError $ "Expected <tr>, but found <" ++ Text.unpack eltName ++ ">"

nodeToTableCell :: Node -> Convert R.TableCell
nodeToTableCell (NodeElement Element {..})
  | eltName == "th"
  = R.TableHeaderCell <$> nodesToBlocks eltChildren
  | eltName == "td"
  = R.TableHeaderCell <$> nodesToBlocks eltChildren
  | otherwise
  = throw $ ConversionError $ "Expected <th> or <td>, but found <" ++ Text.unpack eltName ++ ">"
nodeToTableCell (NodeContent str)
  = pure . R.TableCell . one . R.Para . one . R.Str $ Text.unpack str

nodesToInlines :: [Node] -> Convert [R.Inline]
nodesToInlines = fmap concat . mapM nodeToInlines

nodeToInlines :: Node -> Convert [R.Inline]
nodeToInlines node@(NodeElement {}) =
  withContextM (dumpNode node) (go node)
  where
    go node@(NodeElement (Element {..}))
      ------------- common markup -------------
      | eltName == "b" || eltName == "strong"
      = one . R.Bold <$> nodesToInlines eltChildren
      | eltName == "i" || eltName == "em"
      = one . R.Italic <$> nodesToInlines eltChildren
      | eltName == "tt" || eltName == "code"
      = pure . one . R.Monospaced Nothing . Text.unpack $ textContent node
      | eltName == "sub"
      = one . R.Subscript <$> nodesToInlines eltChildren
      | eltName == "sup"
      = one . R.Superscript <$> nodesToInlines eltChildren
      | eltName == "small"
      = one . R.Small <$> nodesToInlines eltChildren
      | eltName == "del"
      = one . R.Strikethrough <$> nodesToInlines eltChildren

      ------------- inline media -------------
      | eltName == "img"
      = pure . one $ R.Image

      ------------- line break -------------
      | eltName == "br"
      = pure . one $ R.LineBreak

      ------------- cruft -------------
      -- Skip @<span class="icon"></span>@: these are icons on external links,
      -- injected by Trac itself; they'll only get in the way, so we'll skip them.
      | eltName == "span"
      , attrIs eltAttrs "class" "icon"
      = pure []

      ------------- links -------------
      -- * ticket query links (query=...)
      | eltName == "a"
      , Just issueQuery <- takeIssueQuery =<< lookupAttr "href" node
      = (nodesToInlines eltChildren)
        <++>
        pure [ R.Space, R.Str "(Ticket query:", R.Space ]
        <++> 
        ( pure $
            intercalate
              [ R.Str ",", R.Space ]
              [ [ R.Str (key ++ ": " ++ val) ]
              | (key, val) <- issueQuery
              ]
        )
        <++>
        pure [ R.Str ")" ]

      -- * wiki links
      | eltName == "a"
      , Just wikiname <- takeWikiName =<< lookupAttr "href" node
      , attrContains eltAttrs "class" "wiki"
      = pure [R.WikiLink wikiname (Just . map (Text.unpack . textContent) $ eltChildren)]

      -- * missing links
      | eltName == "a"
      , attrContains eltAttrs "class" "missing"
      = nodesToInlines eltChildren

      -- * ticket links
      | eltName == "a"
      , Just ticketNumber <- takeTicketNumber =<< lookupAttr "href" node
      = pure [R.TracTicketLink ticketNumber (Just . map (Text.unpack . textContent) $ eltChildren)]

      -- TODO:
      -- * ticket comment links
      -- * anchors (<a> without href)
      | eltName == "a"
      , Just url <- lookupAttr "href" node
      = pure [R.Link (Text.unpack url) (map (Text.unpack . textContent) $ eltChildren)]

      ------------- known plain text content -------------
      | eltName == "span"
      = nodesToInlines eltChildren

      ------------- unknown/ignored/skipped: text content + warn -------------
      | otherwise
      = do
          writeLogM "SKIPPED" (dumpNode node)
          nodesToInlines eltChildren

nodeToInlines n = pure . textToInlines $ textContent n

dumpNode :: Node -> String
dumpNode (NodeContent str) =
  "[[TEXT]]"
dumpNode (NodeElement Element {..}) =
  printf "<%s%s>...</%s>"
    eltName
    (dumpAttribs eltAttrs)
    eltName

dumpAttribs :: HashMap.HashMap AttrName AttrValue -> String
dumpAttribs attribs =
  concat
    [ printf " %s='%s'" name value
    | (name, value)
    <- sort $ HashMap.toList attribs
    ]

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
textToInlines = one . R.Str . Text.unpack

textContent :: Node -> Text
textContent (NodeContent str) =
  Text.replace "\x200b" "" str
textContent (NodeElement (Element {..})) =
  mconcat . map textContent $ eltChildren
