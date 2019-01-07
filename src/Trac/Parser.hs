{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Trac.Parser where

import Text.Megaparsec hiding (space, some)
import Debug.Trace
-- import Debug.NoTrace

import Control.Monad (void, when, replicateM_)
import Control.Applicative ((<|>), some, optional, empty)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (anyChar, string, char, oneOf, noneOf, satisfy, newline, spaceChar)
import qualified Text.Megaparsec.Char as C
import Control.Monad.Reader
import Text.Printf

import Data.Char (readLitChar, isSpace, isUpper, isLower)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.List (intercalate)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Data.Maybe
import Data.Void

-- * Types

class Walk p c where
  walk :: (c -> Maybe a) -> p -> [a]

instance Walk a b => Walk [a] b where
  walk f = concatMap (walk f)

type Parser = ParsecT Void String (Reader Int)

data Inline = Bold Inlines
             | Italic Inlines
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

data Block = Header Int Inlines [Block]
           | Para Inlines
           | List ListType [[Block]]
           | DefnList [(Inlines, [[Block]])]
           | Code Type String
           | BlockQuote Inlines
           | Discussion [Block]
           | Table [TableRow]
           | HorizontalLine deriving Show

instance Walk Block Block where
  walk f x@(Header _ _ xs) = maybeToList (f x) ++ walk f xs
  walk f x@(List _ xs) = maybeToList (f x) ++ walk f xs
  walk f x@(Discussion xs) = maybeToList (f x) ++ walk f xs
  walk f x@(Table rows) = maybeToList (f x) ++ walk f rows
  walk f x = maybeToList (f x)

instance Walk Block Inline where
  walk f (Header _ xs blocks) = walk f xs ++ concatMap (walk f) blocks
  walk f (Para xs) = walk f xs
  walk f (List _ blocks) = concatMap (walk f) blocks
  walk f (DefnList items) = concatMap (walk f) (map fst items) ++ concatMap (walk f) (concat $ map snd items)
  walk f (BlockQuote xs) = walk f xs
  walk f (Discussion blocks) = concatMap (walk f) blocks
  walk f (Table rows) = concatMap (walk f) rows
  walk f _ = []

type TableRow = [TableCell]

data TableCell = TableCell Blocks
               | TableHeaderCell Blocks
               deriving (Show)

instance Walk TableCell Block where
  walk f (TableCell xs) = walk f xs
  walk f (TableHeaderCell xs) = walk f xs

instance Walk TableCell Inline where
  walk f = concatMap (walk @Block f) . walk Just

type Document = [Block]

data ListType = ListType deriving Show

-- * Normalizing

normalizeNewlines :: String -> String
normalizeNewlines ('\r':'\n':xs) = '\n': normalizeNewlines xs
normalizeNewlines (c:xs) = c : normalizeNewlines xs
normalizeNewlines [] =  "\n"

normalizeTabs :: String -> String
normalizeTabs =
  concatMap $ \case
      '\t' -> "    "
      c -> [c]

-- * Top-level

parseTrac :: Maybe String -> String -> Either (ParseError Char Void) [Block]
parseTrac msrcname s =
  runReader (runParserT document srcname sn) 0
  where
    sn = normalizeTabs . normalizeNewlines $ s
    srcname = fromMaybe "" msrcname

printPos :: String -> SourcePos -> IO ()
printPos s p = do
  let ll = zipWith (\a b -> printf "%3i | %s" (a :: Int) b) [1..] (lines s)
  let ln = pred . unPos . sourceLine $ p
      cn = pred . unPos . sourceColumn $ p
      target = take 1 $ drop ln ll
      contextBefore = drop (ln - 5) $ take (ln - 1) ll
      contextAfter = take 5 $ drop (ln + 1) ll
  mapM_ putStrLn contextBefore
  mapM_ putStrLn target
  putStrLn $ replicate cn ' ' ++ "^"
  mapM_ putStrLn contextAfter

testParser :: Show a => Int -> Parser a -> String -> IO ()
testParser n p s =
  either
    (error . parseErrorPretty)
    (\(pr, pos) -> print pr >> printPos s pos)
    (runReader (runParserT ((,) <$> p <*> getPosition) "" s) n)

-- * Principal parsers

-- ** Document

document :: Parser Document
document = many block

-- ** Block-level

block :: Parser Block
block = dbg "block" $
  many blankline *> block' <* many blankline

block' :: Parser Block
block' = dbg "block'" $ do
  choice
    [ try table
    , list
    -- , discussion
    -- , blockQuote
    , literalBlock
    -- , defnList
    , header
    , para
    ]

list :: Parser Block
list = bulletList

bulletList :: Parser Block
bulletList = do
  n <- ask
  (indented n <|> indented (succ n)) <* many blankline
  where
    indented n = withIndent n $ do
      lookAhead (skipIndentation *> oneOf "*-")
      List ListType <$> some (try bulletListItem)

bulletListItem :: Parser [Block]
bulletListItem = dbg "bulletListItem" $ do
  skipIndentation
  nmin <- captureIndent
  try $ do
    oneOf "*-" *> some (satisfy isSpace)
  nmax <- captureIndent
  firstLine <- lineOfInlines
  n <- length <$> lookAhead (many (satisfy isSpaceNoNL))
  followedBySameLevelItem <- optionBool $ lookAhead (skipIndentation *> oneOf "*-")
  blocks <- if n < nmin || followedBySameLevelItem
              then
                pure []
              else
                (try . indentedBlocks $ min nmax n) <|> pure []
  return $ mergeLineWithBlocks firstLine blocks
  where
    mergeLineWithBlocks ln (Para ils : bs) = 
      Para (ln ++ [Space] ++ ils) : bs
    mergeLineWithBlocks ln bs =
      Para ln : bs

indentedBlocks :: Int -> Parser [Block]
indentedBlocks n =
  withIndent n $ some (try block)

header :: Parser Block
header = do
  marker <- try $ do
    skipIndentation
    some (char '=')
  skipSpaces
  body <- manyTill
            (inlineNoSpace <|> space)
            ((try $ skipSpaces *> string marker *> newline) <|> (try $ skipSpaces *> newline))
  return $ Header (length marker) body []

para :: Parser Block
para = dbg "para" $ do
  skipIndentation
  Para <$> linesOfInlines

literalBlockStart :: Parser (Maybe String)
literalBlockStart = do
  try $ string "{{{"
  mtype <- optional (try (skipSpaces *> string "#!") *> word)
  skipSpaces
  optional newline
  return mtype

literalBlock :: Parser Block
literalBlock = try $ do
  mtype <- literalBlockStart
  contents <- unlines <$> manyTill anyLine (try $ string "}}}" *> (void newline <|> eof))
  return (Code mtype contents)

-- ** Inline

inline :: Parser Inline
inline = dbg "inline" $
  inlineNoSpace <|> space

inlineNoSpace :: Parser Inline
inlineNoSpace =
  dbg "inlineNoSpace" $
  choice
    [ bangEscaped
    , monospaced
    , monospaced2
    , link
    , commentLink
    , commentTicketLink
    , boldC
    , italicC
    , italicW
    , boldW
    , phabLink
    , verbatimWebLink
    , str
    , eq
    ]

lineOfInlines :: Parser [Inline]
lineOfInlines = do
  x <- inlineNoSpace
  xs <- many inline
  optional $ void newline <|> eof
  return (x:xs)

indentedLineOfInlines :: Parser [Inline]
indentedLineOfInlines = do
  skipIndentation
  lineOfInlines

linesOfInlines :: Parser [Inline]
linesOfInlines = do
  ln <- lineOfInlines
  lns <- many (try indentedLineOfInlines)
  return (intercalate [Space] $ ln:lns)

bangEscaped :: Parser Inline
bangEscaped = do
  char '!'
  Str <$> some (satisfy (not . isSpace))

str :: Parser Inline
str = do
  notFollowedBy endOfStr
  Str <$> someTill (satisfy $ not . isSpace) endOfStr
    where
      endOfStr = lookAhead $ choice
          [ void eof
          , void $ satisfy isSpace
          , void $ string "**"
          , void $ string "//"
          , void $ string "''"
          , void $ oneOf "=`"
          , void $ string "{{{"
          ]

eq :: Parser Inline
eq = Str <$> some (char '=')

inlineMarkup :: String -> String -> ([Inline] -> Inline) -> Parser Inline
inlineMarkup opening closing wrap = do
  try (string opening)
  wrap <$> someTill inline (void (try (string closing)))

italicC :: Parser Inline
italicC = inlineMarkup "//" "//" Italic

boldC :: Parser Inline
boldC = inlineMarkup "**" "**" Bold

italicW :: Parser Inline
italicW = inlineMarkup "''" "''" Italic

boldW :: Parser Inline
boldW = inlineMarkup "'''" "'''" Bold

space :: Parser Inline
space = do
  some (satisfy isSpaceNoNL)
  pure Space

singleNewline :: Parser Inline
singleNewline = try $ do
  newline
  skipIndentation
  notFollowedBy $ choice
    [ try . void $ skipSpaces *> newline
    , try . void $ skipSpaces *> oneOf "-*"
    , try . void $ string "{{{"
    , try . void $ char '|'
    ]
  pure Space

monospaced :: Parser Inline
monospaced =
  Monospaced Nothing <$> try (between (char '`') (char '`')
                  (someTill anyChar (lookAhead $ char '`')))

monospaced2 :: Parser Inline
monospaced2 = try . between (string "{{{") (string "}}}") $ do
  Monospaced <$> (optional $ try (many (satisfy isSpace) *> string "#!") *> word <* skipSpaces)
             <*> someTill anyChar (lookAhead $ string "}}}")

-- ** Link parsers
link :: Parser Inline
link = longhandLink <|> shorthandLink

shorthandLink :: Parser Inline
shorthandLink = do
  try (char '[')
  notFollowedBy (satisfy isSpace)
  l <- many (noneOf "]\n ")
  f <- makeLink l
  desc <- words <$> manyTill (noneOf "]\n") (char ']')
  return $ f desc

longhandLink :: Parser Inline
longhandLink = do
  try (string "[[")
  withoutDesc <|> withDesc
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

verbatimWebLink :: Parser Inline
verbatimWebLink = do
  leader <- try (string "http://" <|> string "https://" <|> string "www.")
  remainder <- many (satisfy $ not . isSpace)
  let url = leader ++ remainder
  return $ Link url [url]

tracTicketLink :: Parser Inline
tracTicketLink = 
  try $ TracTicketLink <$> (char '#' *> number) <*> return Nothing

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

table :: Parser Block
table = do
  try $ do
    Table <$> some tableRow
  
tableRow :: Parser TableRow
tableRow = do
  try (string "||" <|> lookAhead (string "{{{#!")) *>
    manyTill (try tableCell <* optional tableRowContinuation)
      (tableRowSeparator <|> void newline)

tableRowContinuation :: Parser ()
tableRowContinuation =
  (try . void $ skipSpaces *> char '\\' *> skipSpaces *> newline *> skipIndentation *> string "||")

tableRowSeparator :: Parser ()
tableRowSeparator = try $ do
  optional newline
  skipIndentation
  char '|'
  count 2 (char '-')
  many (char '-')
  skipSpaces
  newline
  return ()

tableCell :: Parser TableCell
tableCell = tablePPCell <|> tableHeaderCell <|> tableRegularCell

tablePPCell :: Parser TableCell
tablePPCell = do
  ty <- try $ do
    string "{{{#!"
    ty <- (string "td" *> pure TableCell) <|> (string "th" *> pure TableHeaderCell)
    (void $ some (satisfy isSpaceNoNL) *> manyTill anyChar newline) <|> (void newline)
    return ty
  blocks <- withDeeperIndent $ do
    many block
  (try . void $ skipIndentation *> string "}}}" *> newline) <|> eof
  return $ ty blocks

tableHeaderCell :: Parser TableCell
tableHeaderCell = do
  try (string "=")
  TableHeaderCell . (:[]) . Para <$> manyTill inline (try (string "=||") <|> try (string "||"))

tableRegularCell :: Parser TableCell
tableRegularCell = do
  TableCell . (:[]) . Para <$> manyTill inline (string "||")

-- * Link sub-parsers

makeLink :: String -> Parser ([String] -> Inline)
makeLink =
  runSubParser
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

emptyToNothing :: [a] -> Maybe [a]
emptyToNothing [] = Nothing
emptyToNothing xs = Just xs

runSubParser :: Parser b -> String -> Parser b
runSubParser parser str = do
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

-- * Smaller parsers

anyLine :: Parser String
anyLine = do
 v <- manyTill anyChar newline
 return v

optionBool :: Parser a -> Parser Bool
optionBool p = (p *> pure True) <|> pure False

withCapturedIndent :: Parser a -> Parser a
withCapturedIndent p = do
  captureIndent >>= flip withIndent p

captureIndent :: Parser Int
captureIndent = pred . unPos . sourceColumn <$> getPosition

withIndent :: Int -> Parser a -> Parser a
withIndent n = local (const n)

withIncreasedIndentBy :: Int -> Parser a -> Parser a
withIncreasedIndentBy n = local (+ n)

withDeeperIndent :: Parser a -> Parser a
withDeeperIndent p = do
  dn' <- lookAhead $ do
    skipIndentation
    length <$> (some $ satisfy isSpaceNoNL)
  withIncreasedIndentBy dn' p

skipIndentation :: Parser ()
skipIndentation = do
  n <- ask
  void $ count n (satisfy isSpaceNoNL)

blankline :: Parser ()
blankline = try $ many (satisfy isSpaceNoNL) *> void newline

skipSpaces :: Parser ()
skipSpaces = void $ many (satisfy isSpaceNoNL)

isSpaceNoNL :: Char -> Bool
isSpaceNoNL c = isSpace c && c `notElem` "\r\n"
  
number :: Parser Int
number = read <$> some (oneOf "0123456789")

quoted :: Parser a -> Parser a
quoted = between (char '"') (char '"')

stringLit :: Parser String
stringLit = quoted $ many stringLitChar

stringLitChar :: Parser Char
stringLitChar = noneOf "\""

word :: Parser String
word = some (noneOf (reservedChars ++ "\n\r"))

symbol :: Parser Inline
symbol = Str . (: []) <$> oneOf reservedChars

reservedChars :: [Char]
reservedChars = "\'`*/!{}>|[]#: "

-- inlines = some inline
-- 
-- inline :: Parser Inline
-- inline = do
--   notFollowedBy (newline >> newline)
--   inlineNoNL <|> endline
-- 
-- inlineNoNL :: Parser Inline
-- inlineNoNL = do
--              choice [ commentLink
--                     , tracTicketLink
--                     , bold
--                     , italic
--                     , phabLink
--                     , link
--                     , wikiStyle
--                     , monospaced
--                     , monospaced2
--                     , space
--                     , str
--                     , symbol
--                     ]
-- 
-- 
-- bold = try bold1 <|> try bold2
-- bold1 = Bold <$> inlineMarkup 3
-- bold2 = Bold <$> try (between (string "**") (string "**") (some inlineNoNL))
-- 
-- italic = try italic1 <|> italic2
-- italic1 = Italic <$> inlineMarkup 2
-- italic2 = Italic <$> try (between (string "//") (string "//") (some inlineNoNL))
-- 
-- wikiStyle = WikiStyle <$> inlineMarkup 5
-- 
-- inlineMarkup :: Int -> Parser [Inline]
-- inlineMarkup n = try $ do
--   _ <- count n (char '\'')
--   content <- someTill inline (count n (char '\''))
--   return (content)
-- 
-- monospaced :: Parser Inline
-- monospaced =
--   Monospaced Nothing <$> try (between (char '`') (char '`')
--                   (someTill anyChar (lookAhead $ char '`')))
-- 
-- monospaced2 :: Parser Inline
-- monospaced2 = try . between (string "{{{") (string "}}}") $ do
--   Monospaced <$> (optional $ try (many (satisfy isSpace) *> string "#!") *> word <* skipSpaces)
--              <*> someTill anyChar (lookAhead $ string "}}}")
-- 
-- quoted :: Parser a -> Parser a
-- quoted = between (char '"') (char '"')
-- 
-- stringLit :: Parser String
-- stringLit = quoted $ many stringLitChar
-- 
-- stringLitChar :: Parser Char
-- stringLitChar = noneOf "\""
-- 
-- str :: Parser Inline
-- str = Str <$> some (noneOf (reservedChars ++ "\n\r"))
-- 
-- word :: Parser String
-- word = some (noneOf (reservedChars ++ "\n\r"))
-- 
-- symbol :: Parser Inline
-- symbol = Str . (: []) <$> oneOf reservedChars
-- 
-- space :: Parser Inline
-- space = Space <$ oneOf " \t"
-- 
-- number :: Parser Int
-- number = read <$> some (oneOf "0123456789")
-- 
-- tracTicketLink :: Parser Inline
-- tracTicketLink = 
--   try $ TracTicketLink <$> (char '#' *> number) <*> return Nothing
-- 
-- phabLink :: Parser Inline
-- phabLink = try $ string "Phab:D" *> (DifferentialLink <$> number)
-- 
-- commentLink :: Parser Inline
-- commentLink = commentTicketLink <|> ticketCommentLink
-- 
-- commentTicketLink :: Parser Inline
-- commentTicketLink = do
--   c  <- try $ string "comment:" *> number
--   mn <- optional (string ":ticket:" *> number)
--   return $ CommentLink mn c Nothing
-- 
-- ticketCommentLink :: Parser Inline
-- ticketCommentLink = do
--   n <- try $ string "ticket:" *> number
--   mc  <- optional (try (string "#comment:") *> number)
--   case mc of
--     Nothing ->
--       return $ TracTicketLink n Nothing
--     Just c ->
--       return $ CommentLink (Just n) c Nothing
-- 
-- 
-- blankline = oneOf "\n\r"
-- 
-- endline :: Parser Inline
-- endline = try $ do
--   newline
--   notFollowedBy blankline
--   notFollowedBy (skipSpaces *> char '*')
--   notFollowedBy literalBlockStart
--   notFollowedBy (char '>')
--   notFollowedBy (string "  ")
--   notFollowedBy (string "||")
--   return LineBreak
-- 
-- skipSpaces :: Parser ()
-- skipSpaces = () <$ many (oneOf " \t")
-- 
-- blocks =
--   try (many blankline *> eof *> return []) <|>
--   (manyTill (block <* many blankline) eof)
-- 
-- block :: Parser Block
-- block = do
--   many blankline
--   getInput >>= \s -> traceM "block" >>=  \_ -> traceShowM s
--   r <- rawBlock
--   many blankline
--   traceShowM r
--   getInput >>= traceShowM
--   return r
-- 
-- rawBlock :: Parser Block
-- rawBlock =
--   choice
--     [ table
--     , list
--     , discussion
--     , blockQuote
--     , literalBlock
--     , defnList
--     , header
--     , para
--     ]
-- 
-- header :: Parser Block
-- header = try $ do
--   getInput >>= \s -> traceShowM ("header", take 5 $ s)
--   level <- length <$> some (char '=')
--   skipSpaces
--   content <- manyTill inline (try newline <|> try (some (char '=') >> newline))
--   -- bs <- blocks
--   -- _ <- optional $ skipSpaces >> some (char '=')
--   return (Header level content [])
-- 
-- para :: Parser Block
-- para = Para <$> some inline
-- 
-- anyLine :: Parser String
-- anyLine = do
--  v <- manyTill anyChar newline
--  traceShowM v
--  return v
-- 
-- literalBlockStart :: Parser (Maybe String)
-- literalBlockStart = do
--   string "{{{"
--   mtype <- optional (try (many (satisfy isSpace) *> string "#!") *> word)
--   skipSpaces
--   some blankline
--   return mtype
-- 
-- literalBlock :: Parser Block
-- literalBlock = try $ do
--   getInput >>= \s -> traceShowM ("litBlock", take 5 $ s)
--   mtype <- literalBlockStart
--   contents <- unlines <$> manyTill anyLine (string "}}}" *> sc)
--   return (Code mtype contents)
-- 
-- scn :: Parser ()
-- scn = L.space (void spaceChar) empty empty
-- 
-- sc :: Parser ()
-- sc = L.space (void $ oneOf " \t") empty empty
-- 
-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme sc
-- 
-- defnList :: Parser Block
-- defnList = try $ do
--   let getOne = do
--         d <- defnItem
--         many blankline
--         return d
--   DefnList <$> some getOne
-- 
-- 
-- defnItem :: Parser (Inlines, [Inlines])
-- defnItem = try $ do
--  L.indentGuard sc GT pos1
-- -- traceM "here"
--  defn <- someTill inline (lexeme(string "::"))
-- -- traceM "here2"
-- -- traceShowM defn
--  mis <- optional (some inlineNoNL)
--  traceM "here3"
--  traceShowM mis
--  optional newline
--  traceM "here3a"
--  getInput >>= traceShowM
--  traceM "here3b"
-- 
--  iss <- many (do
--           notFollowedBy defnItem
--           L.indentGuard sc GT pos1 >> (some inlineNoNL) <* optional newline)
--  traceM "here3c"
-- -- traceM "here4"
--  traceShowM (defn, mis, iss)
--  return (defn, maybeToList mis ++ iss)
-- 
-- 
-- table :: Parser Block
-- table = do
--   try $ do
--     getInput >>= \s -> traceShowM ("table", s)
--     Table <$> some tableRow
--   
-- tableRow :: Parser TableRow
-- tableRow = do
--   try (string "||") *> manyTill (try tableCell) newline
-- 
-- tableCell :: Parser TableCell
-- tableCell = tableHeaderCell <|> tableRegularCell
-- 
-- tableHeaderCell :: Parser TableCell
-- tableHeaderCell = do
--   try (string "=")
--   TableHeaderCell <$> manyTill inline (try (string "=||") <|> try (string "||"))
-- 
-- tableRegularCell :: Parser TableCell
-- tableRegularCell = do
--   TableCell <$> manyTill inline (string "||")
-- 
-- discussion :: Parser Block
-- discussion = do
--   getInput >>= \s -> traceShowM ("discussion", s)
--   ss <- unlines <$> some (string ">" *> sc *> anyLine)
--   getInput >>= \ (!s) -> traceShowM (s, ss)
--   Discussion <$> runSubParser (many blankline *> many block) ss
-- 
-- data ListType = ListType deriving Show
-- 
-- link :: Parser Inline
-- link = longhandLink <|> shorthandLink
-- 
-- shorthandLink :: Parser Inline
-- shorthandLink = do
--   try (char '[')
--   notFollowedBy (satisfy isSpace)
--   l <- many (noneOf "]\n ")
--   f <- makeLink l
--   desc <- words <$> manyTill (noneOf "]\n") (char ']')
--   return $ f desc
-- 
-- longhandLink :: Parser Inline
-- longhandLink = do
--   try (string "[[")
--   withoutDesc <|> withDesc
--   where
--     withoutDesc = do
--       l <- try $ manyTill anyChar (try $ string "]]")
--       f <- makeLink l
--       let desc = words l
--       return $ f desc
-- 
--     withDesc = do
--       l <- manyTill anyChar (char '|')
--       f <- makeLink l
--       desc <- words <$> manyTill (noneOf "]\n") (string "]]")
--       return $ f desc
-- 
-- emptyToNothing :: [a] -> Maybe [a]
-- emptyToNothing [] = Nothing
-- emptyToNothing xs = Just xs
-- 
-- makeLink :: String -> Parser ([String] -> Inline)
-- makeLink =
--   runSubParser
--     ( try makeCommentLink
--     <|> try makeCommitLink
--     <|> try makeTicketLink
--     <|> try makeWikiLink
--     <|> makeWebLink
--     )
--   where
--     makeCommitLink = do
--       commitInfo <- try (string "changeset:") *> stringLit
--       let commitHash = takeWhile (/= '/') commitInfo
--           mcommitRepo = emptyToNothing . drop 1 . dropWhile (/= '/') $ commitInfo
--       eof
--       return $ GitCommitLink commitHash mcommitRepo
--     makeCommentLink = do
--       commentNumber <- try (string "comment:") *> number
--       ticketNumber <- optional (try (string ":ticket:") *> number)
--       eof
--       return $ CommentLink ticketNumber commentNumber . emptyToNothing
--     makeWikiLink = do
--       (_ :: Maybe String) <- optional (try (string "wiki:"))
--       let ccWord :: Parser String
--           ccWord = do
--             c <- satisfy isUpper
--             cs <- some (satisfy isLower)
--             return $ c:cs
--       let wikiPart = concat <$> some ccWord
--       parts <- try wikiPart `sepBy1` (char '/' :: Parser Char)
--       eof
--       return $ WikiLink (intercalate "/" parts) . emptyToNothing
--     makeTicketLink = do
--       ticketNumber <- try (string "ticket:") *> number
--       commentNumberMay <- optional (try (string "#comment:") *> number)
--       eof
--       case commentNumberMay of
--         Nothing ->
--           return $ TracTicketLink ticketNumber . emptyToNothing
--         Just commentNumber ->
--           return $ CommentLink (Just ticketNumber) commentNumber . emptyToNothing
--     makeWebLink = do
--       url <- manyTill anyChar eof
--       return $ Link url
-- 
-- 
-- 
-- indentBlock2 :: (Show s, MonadParsec e s m, Token s ~ Char)
--   => m ()              -- ^ How to consume indentation (white space)
--   -> m (L.IndentOpt m a b) -- ^ How to parse “reference” token
--   -> m a
-- indentBlock2 sc r = do
--   sc
--   ref <- L.indentLevel
--   traceShowM ref
--   a   <- r
--   getInput >>= \s -> traceShowM ("pItemList after r", s)
--   ref' <- L.indentLevel
--   traceShowM ref'
--   case a of
--     L.IndentNone x -> return x
--     L.IndentMany indent f p -> do
--       mlvl <- optional . try $ C.eol *> L.indentGuard sc EQ ref
--       traceShowM mlvl
--       case mlvl of
--         Nothing  -> sc *> f []
--         Just lvl -> indentedItems ref (fromMaybe lvl indent) sc p >>= f
--     L.IndentSome indent f p -> do
--       lvl <- C.eol *> L.indentGuard sc GT ref
--       indentedItems ref (fromMaybe lvl indent) sc p >>= f
-- 
-- indentedItems :: Show s => MonadParsec e s m
--   => Pos               -- ^ Reference indentation level
--   -> Pos               -- ^ Level of the first indented item ('lookAhead'ed)
--   -> m ()              -- ^ How to consume indentation (white space)
--   -> m b               -- ^ How to parse indented tokens
--   -> m [b]
-- indentedItems ref lvl sc p = do
--   traceShow (ref, lvl) go
--   where
--     go = (sc *> L.indentLevel) >>= re . traceShowId
--     re pos
--       | pos < ref = return []
--       | pos == lvl = (:) <$> try p <*> go
--       | otherwise  = do
--           getInput >>= traceShowM
--           traceShowM (ref, lvl)
--           traceM "otherwise"
--           done <- isJust <$> optional eof
--           if done
--             then return []
--             else L.incorrectIndent EQ lvl pos
-- 
-- blockQuote :: Parser Block
-- blockQuote = try $ do
--   getInput >>= \s -> traceShowM ("blockQuote", take 5 $ s)
--   ss <- unlines <$> some (string "  " *> anyLine)
--   traceM ss
--   BlockQuote <$> runSubParser inlines ss
-- 
-- runSubParser :: Parser b -> String -> Parser b
-- runSubParser parser str = do
--   oldPos <- getPosition
--   oldInput <- getInput
--   setInput str
--   result <- parser
--   traceM "success"
--   skipSpaces
--   traceM "success"
--   eof
--   setInput oldInput
--   setPosition oldPos
--   return result
-- 
-- 
-- 
-- 
-- 
-- list :: Parser Block
-- list = bulletList -- <|> numberedList
-- 
-- bulletList :: Parser Block
-- bulletList = do
--   startPos <- getPosition
--   let n = unPos $ sourceColumn startPos
--       skipIndent = replicateM_ n (satisfy isSpace)
--   firstItem <- try $ bulletListItem n
--   moreItems <- many . try $ skipIndent *> bulletListItem n
--   return $ List ListType (firstItem:moreItems)
--   where
--     bulletListItem n = do
--       char '*'
--       skipSpaces
-- 
--       firstBlock <- rawBlock
--       remainder <- many (try $ do
--         leadingSpace <- many (satisfy isSpace)
--         notFollowedBy (char '*')
--         when (length leadingSpace < n) (fail "Not indented enough")
--         rawBlock)
--       return (firstBlock : remainder)
-- 
--       -- firstLine <- (some (notFollowedBy newline *> inlineNoNL) <* newline)
--       -- remainder <- many
--       --   (try $ do
--       --     leadingSpace <- many (satisfy isSpace)
--       --     notFollowedBy (char '*')
--       --     when (length leadingSpace < n) (fail "Not indented enough")
--       --     some (notFollowedBy newline *> inlineNoNL) <* newline
--       --   )
--       -- return (Para (concat $ firstLine : remainder))
-- 
-- 
-- 
-- 
-- 
-- 
-- 
-- 
-- reservedChars :: [Char]
-- reservedChars = "\'`*/!{}>|[]#: "
-- 
-- 
-- 
-- 
-- 
-- 
