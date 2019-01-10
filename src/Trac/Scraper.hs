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

httpGet :: String -> IO LBS.ByteString
httpGet url = do
  manager <- newTlsManager
  rq <- parseUrlThrow url
  rp <- httpLbs rq manager
  return $ responseBody rp

data ConversionError = ConversionError String
  deriving (Show)

instance Exception ConversionError where

convert :: String -> String -> String -> Maybe Int -> Maybe String -> LookupComment -> LBS.ByteString -> IO String
convert base org proj mn msrcname cm s =
    fmap (writeRemarkup base org proj)
    $ convertBlocks mn cm
    $ nodeToBlocks
    $ maybe (throw $ ConversionError "Main content not found") id
    $ extractPayload
    $ parseHtml s

parseHtml :: LBS.ByteString -> [Node]
parseHtml =
  parseDOM True . LText.pack . LUTF8.toString

extractPayload :: [Node] -> Maybe Node
extractPayload [] =
  Nothing
extractPayload (x:xs) = 
  extractPayloadFrom x <|> extractPayload xs

extractPayloadFrom :: Node -> Maybe Node
extractPayloadFrom (NodeContent _) = Nothing
extractPayloadFrom node@(NodeElement (Element {..}))
  | eltName == "div" && HashMap.lookup "id" eltAttrs == Just "wikipage"
  = Just node
  | otherwise
  = extractPayload eltChildren

nodesToBlocks :: [Node] -> [R.Block]
nodesToBlocks nodes =
  go $ zip (map nodeToInlines nodes) (map nodeToBlocks nodes)
  where
    go ((_, blocks):xs)
      | not (null blocks)
      = blocks ++ go xs
    go []
      = []
    go xs
      = let (is, rem) = break (not . null . snd) xs
        in (R.Para $ concatMap fst is) :
           go rem


nodeToBlocks :: Node -> [R.Block]
nodeToBlocks (NodeContent str) = []
nodeToBlocks (NodeElement elem) = elemToBlocks elem

elemToBlocks :: Element -> [R.Block]
elemToBlocks Element {..}
  | eltName == "ul"
  = [R.List R.BulletListType $ map nodeToLi eltChildren]
  | eltName == "ol"
  = [R.List R.NumberedListType $ map nodeToLi eltChildren]
  | eltName == "pre"
  = let syntaxMay = Text.unpack <$> HashMap.lookup "class" eltAttrs
    in [R.Code syntaxMay $ concatMap textContent eltChildren]
  | eltName == "p"
  = [R.Para $ nodesToInlines eltChildren]
  | eltName == "hr"
  = [R.HorizontalLine]
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
  | eltName == "table"
  = nodesToTable eltChildren
  | eltName == "dl"
  = error "<DL> not supported"
  | eltName `elem` ["div", "section"]
  = nodesToBlocks eltChildren
  | otherwise
  = []

nodesToTable :: [Node] -> [R.Block]
nodesToTable (NodeElement (Element "tbody" _ xs) : ns) =
  nodesToTable xs ++ nodesToTable ns
nodesToTable xs =
  [R.Table $ map nodeToTR xs]

nodeToTR :: Node -> R.TableRow
nodeToTR (NodeContent str) =
  -- Nothing useful we can do here other than dump the string in a
  -- single-cell table row
  [R.TableCell [R.Para [R.Str $ Text.unpack str]]]
nodeToTR (NodeElement Element {..}) =
  map nodeToTableCell eltChildren

nodeToTableCell :: Node -> R.TableCell
nodeToTableCell (NodeElement Element {..})
  | eltName == "th"
  = R.TableHeaderCell $ nodesToBlocks eltChildren
  | otherwise
  = R.TableCell $ nodesToBlocks eltChildren
nodeToTableCell (NodeContent str)
  = R.TableCell [R.Para [R.Str $ Text.unpack str]]

nodeToLi :: Node -> R.Block
nodeToLi node = R.Para $ nodeToInlines node

nodesToInlines :: [Node] -> [R.Inline]
nodesToInlines = concatMap nodeToInlines

nodeToInlines :: Node -> [R.Inline]
nodeToInlines node@(NodeElement (Element {..}))
  | eltName == "b" || eltName == "strong"
  = [R.Bold $ nodesToInlines eltChildren]
  | eltName == "i" || eltName == "em"
  = [R.Italic $ nodesToInlines eltChildren]
  | eltName == "tt" || eltName == "code"
  = [R.Monospaced Nothing $ textContent node]
  | eltName == "img"
  = [R.Image]
  | eltName == "br"
  = [R.LineBreak]
  | otherwise
  = nodesToInlines eltChildren
nodeToInlines n = [R.Str $ textContent n]

textContent :: Node -> String
textContent (NodeContent str) = Text.unpack str
textContent (NodeElement (Element {..})) = concatMap textContent eltChildren
