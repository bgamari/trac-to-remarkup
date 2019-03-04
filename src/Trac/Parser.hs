{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Trac.Parser where

--import Pandoc.Types
import Text.Megaparsec hiding (space, some)
--import Debug.Trace
import Debug.NoTrace

import Control.Applicative (empty)
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (anyChar, string, char, oneOf, noneOf, satisfy, newline, spaceChar)
import qualified Text.Megaparsec.Char as C

import Control.Applicative ((<|>), some, optional)
import Control.Monad (void)
import Data.Char (readLitChar, isSpace, isUpper, isLower)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List (intercalate)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Data.Maybe
import Data.Void

class Walk p c where
  walk :: (c -> Maybe a) -> p -> [a]

instance Walk a b => Walk [a] b where
  walk f = concatMap (walk f)

type Parser = Parsec Void String

normaliseNewlines :: String -> String
normaliseNewlines ('\r':'\n':xs) = '\n': normaliseNewlines xs
normaliseNewlines (c:xs) = c : normaliseNewlines xs
normaliseNewlines [] =  "\n"

data Inline = Bold Inlines
             | Italic Inlines
             | Superscript Inlines
             | Subscript Inlines
             | Small Inlines
             | Strikethrough Inlines
             | WikiStyle Inlines
             | Monospaced Type String
             | Link String [String]
             | WikiLink String (Maybe [String])
             | GitCommitLink String (Maybe String) [String]
             | TracTicketLink Int (Maybe [String])
             | DifferentialLink Int
             | CommentLink (Maybe Int) Int (Maybe [String])
             | Anchor
             | Image
             | Comment
             | Str String
             | LineBreak
             | Space deriving Show

type Inlines = [Inline]

instance Walk Inline Inline where
  walk f x@(Bold xs) = maybeToList (f x) ++ walk f xs
  walk f x@(Italic xs) = maybeToList (f x) ++ walk f xs
  walk f x = maybeToList (f x)

type Blocks = [Block]
type Type = Maybe String

data Block = Header Int Inlines Blocks
           | Para Inlines
           | List ListType [Blocks]
           | DefnList [(Blocks, [Blocks])]
           | Code Type String
           | BlockQuote Inlines
           | Discussion [Block]
           | Table [TableRow]
           | HorizontalLine
           deriving Show

instance Walk Block Block where
  walk f x@(Header _ _ xs) = maybeToList (f x) ++ walk f xs
  walk f x@(List _ xs) = maybeToList (f x) ++ walk f xs
  walk f x@(Discussion xs) = maybeToList (f x) ++ walk f xs
  walk f x = maybeToList (f x)

instance Walk Block Inline where
  walk f (Header _ xs blocks) = walk f xs ++ concatMap (walk f) blocks
  walk f (Para xs) = walk f xs
  walk f (List _ blocks) = concatMap (walk f) blocks
  walk f (DefnList items) = concatMap (walk f) (map fst items) ++ concatMap (walk f) (concat $ map snd items)
  walk f (BlockQuote xs) = walk f xs
  walk f (Discussion blocks) = concatMap (walk f) blocks
  walk f (Table rows) = concatMap (walk f) (concat rows)
  walk f _ = []

type TableRow = [TableCell]

data TableCell = TableCell Blocks
               | TableHeaderCell Blocks
               deriving (Show)

instance Walk TableCell Inline where
  walk f (TableCell xs) = walk f xs
  walk f (TableHeaderCell xs) = walk f xs

type Document = [Block]

parseTrac :: Maybe String -> String -> Either (ParseError Char Void) [Block]
parseTrac msrcname s =
  runParser blocks srcname sn
  where
    sn = normaliseNewlines s
    srcname = fromMaybe "" msrcname


testParser :: Parser a -> String -> a
testParser p s = either (error . show) id (runParser p "" s)

-- | Wrap one inline in a Para
inlineToPara :: Inline -> Block
inlineToPara = Para . (:[])

-- | Wrap many inlines in individual Paras
inlinesToParas :: Inlines -> Blocks
inlinesToParas = map inlineToPara

-- | Wrap many inlines into one Para
inlinesToPara :: Inlines -> Blocks
inlinesToPara = (:[]) . Para

inlines = some inline

inline :: Parser Inline
inline = do
  notFollowedBy (newline >> newline)
  inlineNoNL <|> endline

inlineNoNL :: Parser Inline
inlineNoNL = do
             choice [ commentLink
                    , tracTicketLink
                    , bold
                    , italic
                    , phabLink
                    , link
                    , wikiStyle
                    , monospaced
                    , monospaced2
                    , space
                    , str
                    , symbol
                    ]


bold = try bold1 <|> try bold2
bold1 = Bold <$> inlineMarkup 3
bold2 = Bold <$> try (between (string "**") (string "**") (some inlineNoNL))

italic = try italic1 <|> italic2
italic1 = Italic <$> inlineMarkup 2
italic2 = Italic <$> try (between (string "//") (string "//") (some inlineNoNL))

wikiStyle = WikiStyle <$> inlineMarkup 5

inlineMarkup :: Int -> Parser [Inline]
inlineMarkup n = try $ do
  _ <- count n (char '\'')
  content <- someTill inline (count n (char '\''))
  return (content)

monospaced :: Parser Inline
monospaced =
  Monospaced Nothing <$> try (between (char '`') (char '`')
                  (someTill anyChar (lookAhead $ char '`')))

monospaced2 :: Parser Inline
monospaced2 = try . between (string "{{{") (string "}}}") $ do
  Monospaced <$> (optional $ try (many (satisfy isSpace) *> string "#!") *> word <* skipSpaces)
             <*> someTill anyChar (lookAhead $ string "}}}")

quoted :: Parser a -> Parser a
quoted = between (char '"') (char '"')

stringLit :: Parser String
stringLit = quoted $ many stringLitChar

stringLitChar :: Parser Char
stringLitChar = noneOf "\""

str :: Parser Inline
str = Str <$> some (noneOf (reservedChars ++ "\n\r"))

word :: Parser String
word = some (noneOf (reservedChars ++ "\n\r"))

symbol :: Parser Inline
symbol = Str . (: []) <$> oneOf reservedChars

space :: Parser Inline
space = Space <$ oneOf " \t"

number :: Parser Int
number = read <$> some (oneOf "0123456789")

tracTicketLink :: Parser Inline
tracTicketLink = numberStyle <|> httpStyle <|> httpsStyle
  where
    numberStyle = try $ TracTicketLink <$> (char '#' *> number) <*> return Nothing
    httpStyle = try $ TracTicketLink <$> (string "http://ghc.haskell.org/trac/ghc/ticket/" *> number) <*> return Nothing
    httpsStyle = try $ TracTicketLink <$> (string "https://ghc.haskell.org/trac/ghc/ticket/" *> number) <*> return Nothing

phabLink :: Parser Inline
phabLink = try $ string "Phab:D" *> (DifferentialLink <$> number)

commentLink :: Parser Inline
commentLink = commentTicketLink <|> ticketCommentLink

commentTicketLink :: Parser Inline
commentTicketLink = do
  c  <- try $ string "comment:" *> number
  mn <- optional (string ":ticket:" *> number)
  return $ CommentLink mn c Nothing

ticketCommentLink :: Parser Inline
ticketCommentLink = do
  n <- try $ string "ticket:" *> number
  mc  <- optional (try (string "#comment:") *> number)
  case mc of
    Nothing ->
      return $ TracTicketLink n Nothing
    Just c ->
      return $ CommentLink (Just n) c Nothing


blankline = oneOf "\n\r"

endline :: Parser Inline
endline = try $ do
  newline
  notFollowedBy blankline
  notFollowedBy (pItemListStart)
  notFollowedBy (pNumberListStart)
  notFollowedBy literalBlockStart
  notFollowedBy (char '>')
  notFollowedBy (string "  ")
  notFollowedBy (string "||")
  return LineBreak

skipSpaces :: Parser ()
skipSpaces = () <$ many (oneOf " \t")

blocks =
  try (many blankline *> eof *> return []) <|>
  (manyTill (block <* many blankline) eof)

block :: Parser Block
block = do
  many blankline
  getInput >>= \s -> traceM "block" >>=  \_ -> traceShowM s
  r <- choice
          [ table
          , pList
          , discussion
          , blockQuote
          , literalBlock
          , defnList
          , header
          , para
          ]
  many blankline
  traceShowM r
  getInput >>= traceShowM
  return r

header :: Parser Block
header = try $ do
  getInput >>= \s -> traceShowM ("header", take 5 $ s)
  level <- length <$> some (char '=')
  skipSpaces
  content <- manyTill inline (try newline <|> try (some (char '=') >> newline))
  -- bs <- blocks
  -- _ <- optional $ skipSpaces >> some (char '=')
  return (Header level content [])

para :: Parser Block
para = Para <$> some inline

anyLine :: Parser String
anyLine = do
 v <- manyTill anyChar newline
 traceShowM v
 return v

literalBlockStart :: Parser (Maybe String)
literalBlockStart = do
  string "{{{"
  mtype <- optional (try (many (satisfy isSpace) *> string "#!") *> word)
  skipSpaces
  some blankline
  return mtype

literalBlock :: Parser Block
literalBlock = try $ do
  getInput >>= \s -> traceShowM ("litBlock", take 5 $ s)
  mtype <- literalBlockStart
  contents <- unlines <$> manyTill anyLine (string "}}}" *> sc)
  return (Code mtype contents)

scn :: Parser ()
scn = L.space (void spaceChar) empty empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parser :: Parser [Block]
-- parser = pItemListStart *> pItemList <* eof

defnList :: Parser Block
defnList = try $ do
  let getOne = do
        d <- defnItem
        many blankline
        return d
  DefnList <$> some getOne


defnItem :: Parser (Blocks, [Blocks])
defnItem = try $ do
 L.indentGuard sc GT pos1
-- traceM "here"
 defn <- inlinesToPara <$> someTill inline (lexeme(string "::"))
-- traceM "here2"
-- traceShowM defn
 mbs <- optional (inlinesToPara <$> some inlineNoNL)
 traceM "here3"
 traceShowM mbs
 optional newline
 traceM "here3a"
 getInput >>= traceShowM
 traceM "here3b"

 iss <- many (do
          notFollowedBy defnItem
          L.indentGuard sc GT pos1 >> (some inlineNoNL) <* optional newline
          )
 let bss :: [Blocks]
     bss = map inlinesToPara iss
 traceM "here3c"
-- traceM "here4"
 traceShowM (defn, mbs, bss)
 return (defn, maybeToList mbs ++ bss)


table :: Parser Block
table = do
  try $ do
    getInput >>= \s -> traceShowM ("table", s)
    Table <$> some tableRow
  
tableRow :: Parser TableRow
tableRow = do
  try (string "||") *> manyTill (try tableCell) newline

tableCell :: Parser TableCell
tableCell = tableHeaderCell <|> tableRegularCell

tableHeaderCell :: Parser TableCell
tableHeaderCell = do
  try (string "=")
  TableHeaderCell . inlinesToPara <$>
    manyTill inline (try (string "=||") <|> try (string "||"))

tableRegularCell :: Parser TableCell
tableRegularCell = do
  TableCell . inlinesToPara <$>
    manyTill inline (string "||")

discussion :: Parser Block
discussion = do
  getInput >>= \s -> traceShowM ("discussion", s)
  ss <- unlines <$> some (string ">" *> sc *> anyLine)
  getInput >>= \ (!s) -> traceShowM (s, ss)
  Discussion <$> parseFromString (many blankline *> many block) ss

data ListType
  = BulletListType
  | NumberedListType
  deriving Show

pList :: Parser Block
pList = do
  try pNumberList <|> try pItemList

pItemList :: Parser Block
pItemList = do
  getInput >>= \s -> traceShowM ("pItemList", s)
  List BulletListType <$> (some $ try pItemListStart *> pItemListCont)

pNumberList :: Parser Block
pNumberList = do
  getInput >>= \s -> traceShowM ("pNumberList", s)
  List NumberedListType <$> (some $ try pNumberListStart *> pNumberListCont)

pItemListStart = try $ sc *> char '*'

pItemListCont :: Parser [Block]
pItemListCont = do
  getInput >>= \s -> traceShowM ("pItemList", s)
  indentBlock2 scn p
  where
    p :: Parser (L.IndentOpt Parser [Block] Block)
    p = do
      getInput >>= traceShowM
      s <- Para <$> some (try inlineNoNL)
      traceShowM ("s", (show s))
      return (L.IndentMany Nothing (\ss -> return (s:ss))
                (Para <$> some inlineNoNL <|> pItemList))

pNumberListStart = try $ sc *> many (oneOf ['0'..'9']) *> char '.'

pNumberListCont :: Parser [Block]
pNumberListCont = do
  getInput >>= \s -> traceShowM ("pNumberList", s)
  indentBlock2 scn p
  where
    p :: Parser (L.IndentOpt Parser [Block] Block)
    p = do
      getInput >>= traceShowM
      s <- Para <$> some (try inlineNoNL)
      traceShowM ("s", (show s))
      return (L.IndentMany Nothing (\ss -> return (s:ss))
                (Para <$> some inlineNoNL <|> pNumberList))

link :: Parser Inline
link = try longhandLink <|> try shorthandLink

shorthandLink :: Parser Inline
shorthandLink = do
  char '['
  notFollowedBy (satisfy isSpace)
  -- People sometimes use square brackets in prose; only accept things that
  -- look like they begin with a link target
  prefix <- string "http:" <|> string "https:" <|> string "wiki:" <|> string "ticket:"
  l <- many (noneOf "]\n ")
  f <- makeLink $ prefix++l
  desc <- words <$> manyTill (noneOf "]\n") (char ']')
  return $ f desc

longhandLink :: Parser Inline
longhandLink = do
  try (string "[[")
  withDesc <|> withoutDesc
  where
    withoutDesc = do
      l <- try $ manyTill anyChar (try $ string "]]")
      f <- makeLink l
      let desc = words l
      return $ f desc

    withDesc = do
      l <- manyTill anyChar (char '|')
      f <- makeLink l
      desc <- words <$> manyTill (noneOf "]\n") (string "]]")
      return $ f desc

emptyToNothing :: [a] -> Maybe [a]
emptyToNothing [] = Nothing
emptyToNothing xs = Just xs

makeLink :: String -> Parser ([String] -> Inline)
makeLink =
  parseFromString
    ( try makeCommentLink
    <|> try makeCommitLink
    <|> try makeTicketLink
    <|> try makeWikiLink
    <|> makeWebLink
    )
  where
    makeCommitLink = do
      commitInfo <- try (string "changeset:") *> stringLit
      let commitHash = takeWhile (/= '/') commitInfo
          mcommitRepo = emptyToNothing . drop 1 . dropWhile (/= '/') $ commitInfo
      eof
      return $ GitCommitLink commitHash mcommitRepo
    makeCommentLink = do
      commentNumber <- try (string "comment:") *> number
      ticketNumber <- optional (try (string ":ticket:") *> number)
      eof
      return $ CommentLink ticketNumber commentNumber . emptyToNothing
    makeWikiLink = do
      (_ :: Maybe String) <- optional (try (string "wiki:"))
      let ccWord :: Parser String
          ccWord = do
            c <- satisfy isUpper
            cs <- some (satisfy isLower)
            return $ c:cs
      let wikiPart = concat <$> some ccWord
      parts <- try wikiPart `sepBy1` (char '/' :: Parser Char)
      eof
      return $ WikiLink (intercalate "/" parts) . emptyToNothing
    makeTicketLink = do
      ticketNumber <- try (string "ticket:") *> number
      commentNumberMay <- optional (try (string "#comment:") *> number)
      eof
      case commentNumberMay of
        Nothing ->
          return $ TracTicketLink ticketNumber . emptyToNothing
        Just commentNumber ->
          return $ CommentLink (Just ticketNumber) commentNumber . emptyToNothing
    makeWebLink = do
      url <- manyTill anyChar eof
      return $ Link url



indentBlock2 :: (Show s, MonadParsec e s m, Token s ~ Char)
  => m ()              -- ^ How to consume indentation (white space)
  -> m (L.IndentOpt m a b) -- ^ How to parse “reference” token
  -> m a
indentBlock2 sc r = do
  sc
  ref <- L.indentLevel
  traceShowM ref
  a   <- r
  getInput >>= \s -> traceShowM ("pItemList after r", s)
  ref' <- L.indentLevel
  traceShowM ref'
  case a of
    L.IndentNone x -> return x
    L.IndentMany indent f p -> do
      mlvl <- optional . try $ C.eol *> L.indentGuard sc EQ ref
      traceShowM mlvl
      case mlvl of
        Nothing  -> sc *> f []
        Just lvl -> indentedItems ref (fromMaybe lvl indent) sc p >>= f
    L.IndentSome indent f p -> do
      lvl <- C.eol *> L.indentGuard sc GT ref
      indentedItems ref (fromMaybe lvl indent) sc p >>= f

indentedItems :: Show s => MonadParsec e s m
  => Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Level of the first indented item ('lookAhead'ed)
  -> m ()              -- ^ How to consume indentation (white space)
  -> m b               -- ^ How to parse indented tokens
  -> m [b]
indentedItems ref lvl sc p = do
  traceShow (ref, lvl) go
  where
    go = (sc *> L.indentLevel) >>= re . traceShowId
    re pos
      | pos < ref = return []
      | pos == lvl = (:) <$> try p <*> go
      | otherwise  = do
          getInput >>= traceShowM
          traceShowM (ref, lvl)
          traceM "otherwise"
          done <- isJust <$> optional eof
          if done
            then return []
            else L.incorrectIndent EQ lvl pos

blockQuote :: Parser Block
blockQuote = try $ do
  getInput >>= \s -> traceShowM ("blockQuote", take 5 $ s)
  ss <- unlines <$> some (string "  " *> anyLine)
  traceM ss
  BlockQuote <$> parseFromString inlines ss

parseFromString :: Parser b -> String -> Parser b
parseFromString parser str = do
  oldPos <- getPosition
  oldInput <- getInput
  setInput str
  result <- parser
  traceM "success"
  skipSpaces
  traceM "success"
  eof
  setInput oldInput
  setPosition oldPos
  return result

reservedChars :: [Char]
reservedChars = "\'`*/!{}>|[]#: "
