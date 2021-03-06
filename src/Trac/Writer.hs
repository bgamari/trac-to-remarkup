{-# LANGUAGE OverloadedStrings #-}

module Trac.Writer where

import Data.List
import Data.String (IsString, fromString)
import qualified Data.Text as T
import Trac.Pretty
import qualified Data.Map as M
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)

type CommentMap = M.Map (Int, Int) Int

data Context
  = Context
      { ctxOrg :: String
      , ctxProject :: String
      , ctxBaseUrl :: String
      }

type W a = Reader Context a

runW :: Context -> W a -> a
runW = flip runReader

data Inline = Bold Inlines
             | Italic Inlines
             | Monospaced (Maybe String) String
             | Deleted Inlines
             | Superscript Inlines
             | Subscript Inlines
             | Small Inlines
             | Underlined Inlines
             | Highlighted Inlines
             | ObjectLink Ref
             | TicketLink
                  (Maybe Inlines) -- label
                  Int -- ticket number
                  (Maybe Int) -- comment number
             | DifferentialLink Int -- Differential number
             | Str String
             | Space
             | ImageLink
             | UserMention
             | ProjectMention
             | WikiLink Inlines String
             | WebLink Inlines String
             | GitCommitLink Inlines String (Maybe String)
             | LineBreak
             deriving (Show)

type Inlines = [Inline]

data Ref = Ref
  deriving (Show)

type Blocks = [Block]

data Style = BulletStyle
           | NumberedStyle
  deriving (Show)

data Block = Header Int Inlines Blocks
           | Quote Blocks
           | Para Inlines
           | List Style [Blocks]
           | CodeBlock (Maybe Lang) String
           | Table [TableRow]
           | HorizontalLine
           deriving (Show)

type Lang = String

type TableRow = [TableCell]

data TableCell = TableHeaderCell Blocks
               | TableCell Blocks
               deriving (Show)

tableCellContents :: TableCell -> Blocks
tableCellContents (TableCell bs) = bs
tableCellContents (TableHeaderCell bs) = bs


writeRemarkup :: String -> String -> String -> [Block] -> String
writeRemarkup base org project =
  render Nothing . runW (Context org project base) . blocks

blocks :: [Block] -> W Doc
blocks bs =
  foldr (<>) empty . intersperse blankline <$> mapM block bs

repeatP :: Int -> Doc -> Doc
repeatP n s = foldr (<>) empty (replicate n s)

equals = char '='

block :: Block -> W Doc
block (Header n h bs) = do
  let level = repeatP n (char '#')
  heading <- inlines h
  body <- blocks bs
  return $ level <+> heading $+$ body
block (Quote is) =
  prefixed "> " <$> blocks is
block (Para is) = inlines is
block (List BulletStyle iss) =
  vcat <$> mapM (oneListItem "-") iss
block (List NumberedStyle iss) =
  vcat <$> mapM (oneListItem "1.") iss
block (CodeBlock ml s) =
  return $ vcat [text "```" <> mlang, text s, text "```"]
  where
    mlang = case ml of
      Just lang -> text lang
      Nothing -> empty
block (Table rs) =
  table rs
block HorizontalLine = return $ text "---"

table :: [TableRow] -> W Doc
table rs
  -- | isProper rs = niceTable rs'
  | otherwise = htmlTable rs'
  where
    rs' = normalizeTable rs

isProper :: [TableRow] -> Bool
isProper [] = True
isProper (x:xs) =
  (isProperHeaderRow x || isProperBodyRow x) && all isProperBodyRow xs

isProperHeaderRow :: TableRow -> Bool
isProperHeaderRow = all isHeaderCell

isProperBodyRow :: TableRow -> Bool
isProperBodyRow = all (not . isHeaderCell)

isHeaderCell :: TableCell -> Bool
isHeaderCell (TableHeaderCell {}) = True
isHeaderCell (TableCell {}) = False

-- | Render a \"nice\" table (native remarkup)
niceTable :: [TableRow] -> W Doc
niceTable = fmap vcat . mapM niceRow

-- | Normalize the table so that all rows have the same column count.
normalizeTable :: [TableRow] -> [TableRow]
normalizeTable rows =
  map doOne rows
  where
    columns = maximum (1 : map length rows)
    doOne :: TableRow -> TableRow
    doOne xs =
      -- If the row contains only header cells, pad it with header cells,
      -- otherwise pad it with regular cells.
      --
      -- We pick this choice in order to preserve "proper-ness" of tables, that
      -- is, if a table meets the criteria for rendering in remarkup style
      -- before normalization, it should also meet the criteria after
      -- normalization; for "improper" tables, the padding choice doesn't
      -- matter much, but it is reasonable to assume that if a row contains
      -- any non-header cells, then padding it with more non-header cells is
      -- acceptable.
      let emptyCell =
            if not (null xs) && isProperHeaderRow xs then
              TableHeaderCell []
            else
              TableCell []
      in take columns $ xs ++ repeat emptyCell

niceRow :: TableRow -> W Doc
niceRow row =
  if isProperHeaderRow row then
    vcat <$> sequence [ niceRowCells row, niceHeaderUnderline (length row) ]
  else
    niceRowCells row

niceHeaderUnderline :: Int -> W Doc
niceHeaderUnderline n =
  return $ niceRowRaw (replicate n $ text "-----")

niceRowCells :: [TableCell] -> W Doc
niceRowCells cells =
  niceRowRaw <$> mapM (blocks . tableCellContents) cells

niceRowRaw :: [Doc] -> Doc
niceRowRaw items =
  cat (text "|" : [ text " " <> i <> text " |" | i <- items ])

htmlElem :: String -> Doc -> Doc
htmlElem tagName =
  inside (text ("<" ++ tagName ++ ">")) (text ("</" ++ tagName ++ ">"))

-- | Render an HTML-style table (using HTML tags)
htmlTable :: [TableRow] -> W Doc
htmlTable rs =
  htmlElem "table" <$> htmlTableRows rs

htmlTableRows :: [TableRow] -> W Doc
htmlTableRows = fmap vcat . mapM htmlTableRow

htmlTableRow :: TableRow -> W Doc
htmlTableRow tr =
  htmlElem "tr" <$> htmlTableCells tr

htmlTableCells :: [TableCell] -> W Doc
htmlTableCells = fmap vcat . mapM htmlTableCell

htmlTableCell :: TableCell -> W Doc
htmlTableCell (TableHeaderCell bs) =
  htmlElem "th" <$> blocks bs
htmlTableCell (TableCell bs) =
  htmlElem "td" <$> blocks bs

oneListItem :: String -> Blocks -> W Doc
oneListItem mark is =
  hang 2 (text $ mark ++ " ") <$> blocks is
  
  -- (text mark $$) . (nest 4) <$> blocks is

inlines = fmap hcat . mapM inline

inline :: Inline -> W Doc
inline (Bold is) = inside (text "**") (text "**") <$> inlines is
inline (Italic is) = inside (text "*") (text "*") <$> inlines is
inline (Monospaced _ is) = return $ inside (text "`") (text "`") (text is)
inline (Deleted is) = inside (text "~~") (text "~~") <$> inlines is
inline (Underlined is) = inside (text "__") (text "__") <$> inlines is
inline (Highlighted is) = inside (text "!!") (text "!!") <$> inlines is
inline (Small is) = inside (text "<small>") (text "</small>") <$> inlines is
inline (Superscript is) = inside (text "<sup>") (text "</sup>") <$> inlines is
inline (Subscript is) = inside (text "<sub>") (text "</sub>") <$> inlines is
inline (Str str) = return . text . escapeMarkdown $ str
inline Space = return $ space
inline (WebLink is url) = longLink url is
inline (ObjectLink Ref) = return empty
inline (ImageLink) = return empty
inline LineBreak = return cr
inline (WikiLink label url) = longLink url label
inline (TicketLink Nothing n Nothing) =
  -- shorthand ticket link: we can do this nicely
  return $ char '#' <> text (show n)
inline (TicketLink (Just label) n Nothing) = do
  base <- asks ctxBaseUrl
  org <- asks ctxOrg
  proj <- asks ctxProject
  let url = base <> "/" <> org <> "/" <> proj <> "/issues/" <> show n
  longLink url label
inline (TicketLink mlabel n (Just c)) = do
  base <- asks ctxBaseUrl
  org <- asks ctxOrg
  proj <- asks ctxProject
  let url = base <> "/" <> org <> "/" <> proj <> "/issues/" <> show n <> "#note_" <> show c
  case mlabel of
    Just label -> longLink url label
    Nothing    -> let label = "ticket:" <> show n <> "#" <> "comment:" <> show c
                  in longLink url [Str label]
inline (GitCommitLink is hash mrepo) = do
  base <- asks ctxBaseUrl
  org <- asks ctxOrg
  proj <- asks ctxProject
  let repo = fromMaybe proj mrepo
  let url = base <> "/" <> org <> "/" <> repo <> "/commit/" <> hash
  longLink url is


inline (DifferentialLink d) = pure $ mkDifferentialLink d

inline _ = return $ empty

longLink :: String -> [Inline] -> W Doc
longLink url label =
  (<>) <$> (brackets <$> inlines label)
       <*> (parens <$> pure (text url))

mkDifferentialLink :: (Semigroup t, IsString t) => Int -> t
mkDifferentialLink n =
    "[D" <> fromString (show n) <> "](" <> mkDifferentialUrl n <> ")"

mkDifferentialUrl :: (Semigroup t, IsString t) => Int -> t
mkDifferentialUrl n =
    "https://phabricator.haskell.org/D" <> fromString (show n)

-- | Escape special characters for Markdown.
-- Shamelessly ripped out of pandoc and adapted for our needs.
escapeMarkdown :: String -> String
escapeMarkdown = go
  where
  go [] = []
  go (c:cs) =
    case c of
       '<' -> '\\' : '<' : go cs
       '>' -> '\\' : '>' : go cs
       '@' -> case cs of
                    (d:_)
                      | isAlphaNum d || d == '_'
                         -> '\\':'@':go cs
                    _ -> '@':go cs
       _ | c `elem` ['\\','`','*','[',']','#', '|', '^', '~'] ->
              '\\':c:go cs
       _   -> c : go cs
