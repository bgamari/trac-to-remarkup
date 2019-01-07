{-#LANGUAGE LambdaCase #-}

module Trac.Convert
          ( convert
          , convertIgnoreErrors
          , CommentMap
          , LookupComment
          , indentShowed
          )
where

import qualified Trac.Parser as R
import Trac.Writer
import Trac.Db.Types
import Data.List
import qualified Data.Map as M
import Debug.Trace
import Data.Maybe
import Control.Applicative
import Control.Exception
import Control.Monad.State
import Text.Megaparsec.Error (ParseError, parseErrorPretty)
import Data.Void
import Text.Printf
import Debug.Trace (trace)
import Data.Char (isSpace)

type LookupComment = Int -> Int -> IO CommentRef

tee :: (a -> String) -> a -> a
tee f x = trace (f x) x

teeM :: Monad m => (a -> String) -> m a -> m a
teeM f a = do
  x <- a
  traceM (f x)
  return x

teeShow :: Show a => a -> a
teeShow = tee show

convert :: String -> String -> String -> Maybe Int -> Maybe String -> LookupComment -> String -> IO String
convert base org proj mn msrcname cm s =
    fmap (writeRemarkup base org proj)
    $ teeM (\x -> "CONVERT RESULT:\n" ++ indentShowed (show x))
    $ convertBlocks mn cm
    -- $ tee (\x -> "PARSE RESULT:\n" ++ indentShowed (show x))
    $ either throw id
    $ R.parseTrac (msrcname <|> (("ticket:" ++) . show <$> mn))
    $ s

indentShowed :: String -> String
indentShowed =
  unlines .
  collapse .
  indent .
  parse
  where
    breakers = "[](){},;"
    collapsibles = "[({,;"

    indentPlus = "[({"
    indentMinus = "])}"

    isCollapsible c = isSpace c || c `elem` collapsibles

    collapse :: [String] -> [String]
    collapse (x:y:xs)
      | all isCollapsible x =
          collapse $ (x ++ " " ++ dropWhile isSpace y):xs
      | otherwise =
          x:collapse (y:xs)
    collapse xs = xs

    indent lns = flip evalState 0 $ forM lns $ \ln -> do
      let plus = length . filter (`elem` indentPlus) $ ln
      let minus = length . filter (`elem` indentMinus) $ ln
      modify (+ plus)
      i <- get
      modify (subtract minus)
      return $ replicate (i * 2) ' ' ++ ln


    parse = split

    split :: String -> [String]
    split "" = []
    split (x:xs)
      | x == '"' =
          split (skipStringLit xs)
      | isSpace x =
          split xs
      | x `elem` breakers =
          case split xs of
            ((xx@(x2:_)):xxs) | x2 `elem` breakers ->
              [x]:xx:xxs
            (xx:xxs) ->
              (x:' ':xx):xxs
            _ -> [[x]]
    split xs =
      let (lhs, rhs) = break (`elem` breakers) xs
      in lhs:split rhs

    skipStringLit :: String -> String
    skipStringLit [] = []
    skipStringLit ('\\':xs) = skipStringLit (drop 1 xs)
    skipStringLit ('"':xs) = xs
    skipStringLit (x:xs) = skipStringLit xs

convertIgnoreErrors :: String -> String -> String -> Maybe Int -> Maybe String -> LookupComment -> String -> IO String
convertIgnoreErrors base org proj mn msrcname cm s =
  convert base org proj mn msrcname cm s
    `catches`
      [ Handler handleParseError
      , Handler handleOtherError
      ]
  where
    handleOtherError :: SomeException -> IO String
    handleOtherError err = do
      putStrLn "CONVERSION FAILED: OTHER ERROR"
      putStrLn $ displayException err
      return $
        printf "Error:\n\n```\n%s\n```\n\nOriginal Trac source:\n\n```trac\n%s\n```\n"
          (displayException err) s

    handleParseError :: ParseError Char Void -> IO String
    handleParseError err = do
      putStrLn "CONVERSION FAILED: PARSER ERROR"
      putStrLn $ parseErrorPretty err
      writeRemarkup base org proj <$>
        convertBlocks mn cm
          [R.Header 1
            [R.Str "PARSER ERROR:"]
            [ R.Para [R.Str $ parseErrorPretty err]
            , R.Code (Just "trac") s
            ]
          ]


-- (\err -> [R.Para [R.Str "NO PARSE: ", R.Str $ show err, R.Str s]])

-- convertWithError org proj n cm s = writeRemarkup org proj . convertBlocks n cm <$> R.parseTrac s

convertBlocks :: Maybe Int -> LookupComment -> [R.Block] -> IO [Block]
convertBlocks n cm = mapM (convertBlock n cm)

convertBlock :: Maybe Int -> LookupComment -> R.Block -> IO Block
convertBlock n cm (R.Header nlev is bs)
  = Header nlev <$> convertInlines n cm is <*> convertBlocks n cm bs
convertBlock n cm (R.Para is)        = Para <$> convertInlines n cm is
convertBlock n cm (R.List _ bs)      = List Style <$> mapM (convertBlocks n cm) bs
convertBlock n cm (R.DefnList d)     = convertDefnListToTable n cm d
convertBlock n cm (R.Code ty s)      = pure $ CodeBlock ty s
convertBlock n cm (R.BlockQuote bs)  = Quote . (:[]) . Para <$> convertInlines n cm bs
convertBlock n cm (R.Discussion bs)  = Quote <$> convertBlocks n cm bs
convertBlock n cm (R.Table rs)       = Table <$> convertTableRows n cm rs
convertBlock _ _ R.HorizontalLine    = pure HorizontalLine

convertTableRows :: Maybe Int -> LookupComment -> [R.TableRow] -> IO [TableRow]
convertTableRows n cm = mapM (convertTableRow n cm)

convertTableRow :: Maybe Int -> LookupComment -> R.TableRow -> IO TableRow
convertTableRow n cm = mapM (convertTableCell n cm)

convertTableCell :: Maybe Int -> LookupComment -> R.TableCell -> IO TableCell
convertTableCell n cm (R.TableHeaderCell is) = TableHeaderCell <$> convertBlocks n cm is
convertTableCell n cm (R.TableCell is) = TableCell <$> convertBlocks n cm is

convertDefnListToTable :: Maybe Int -> LookupComment -> [(R.Inlines, [R.Blocks])] -> IO Block
convertDefnListToTable n cm [] = pure $ Para []
convertDefnListToTable n cm items = Table . mconcat <$> mapM (convertDefnToTableRows n cm) items

convertDefnToTableRows :: Maybe Int -> LookupComment -> (R.Inlines, [R.Blocks]) -> IO [TableRow]
convertDefnToTableRows n cm (dh, []) = (:[]) . (:[]) . TableHeaderCell . (:[]) . Para <$> convertInlines n cm dh
convertDefnToTableRows n cm (dh, dd:dds) = (:) <$> convertFirstDefnRow n cm dh dd <*> convertAdditionalDefnRows n cm dds

convertFirstDefnRow :: Maybe Int -> LookupComment -> R.Inlines -> R.Blocks -> IO TableRow
convertFirstDefnRow n cm dh dd =
  sequence
    [ TableHeaderCell . (:[]) . Para <$> convertInlines n cm dh
    , TableCell <$> convertBlocks n cm dd
    ]

convertAdditionalDefnRow :: Maybe Int -> LookupComment -> R.Blocks -> IO TableRow
convertAdditionalDefnRow n cm dd =
  sequence
    [ pure $ TableCell []
    , TableCell <$> convertBlocks n cm dd
    ]

convertAdditionalDefnRows :: Maybe Int -> LookupComment -> [R.Blocks] -> IO [TableRow]
convertAdditionalDefnRows n cm = mapM (convertAdditionalDefnRow n cm)

convertInlines n cm = mapM (convertInline n cm)

prettyCommit :: CommitHash -> Maybe RepoName -> [Inline]
prettyCommit hash Nothing = [Str $ take 7 hash]
prettyCommit hash (Just repo) = [Str repo, Str ":", Str $ take 7 hash]

convertInline :: Maybe Int -> LookupComment -> R.Inline -> IO Inline
convertInline n cm (R.Bold is) = Bold <$> convertInlines n cm is
convertInline n cm (R.Monospaced ty is) = pure (Monospaced ty is)
convertInline n cm (R.Italic is) = Italic <$> convertInlines n cm is
convertInline n cm (R.WikiStyle is) = Italic <$> convertInlines n cm is
convertInline n cm (R.Link url []) = pure $ WebLink (intersperse Space [Str url]) url
convertInline n cm (R.Link url is) = pure $ WebLink (intersperse Space (map Str is)) url
convertInline n cm (R.WikiLink wikiname mlabel) = do
  let url = tracWikiNameToGitlab wikiname
      label = map Str $ fromMaybe [wikiname] mlabel
  pure $ WikiLink label url
convertInline n cm (R.GitCommitLink hash mrepo []) = pure $ GitCommitLink (intersperse Space (prettyCommit hash mrepo)) hash mrepo
convertInline n cm (R.GitCommitLink hash mrepo is) = pure $ GitCommitLink (intersperse Space (map Str is)) hash mrepo
convertInline n cm (R.Str s) = pure $ Str s
convertInline _ _ (R.LineBreak)  = pure LineBreak
convertInline _ _ (R.Space)      = pure Space
convertInline _ _ (R.DifferentialLink n) =
  pure $ DifferentialLink n
convertInline _ _ (R.TracTicketLink n desc) =
  pure $ TicketLink (fmap (map Str) desc) n Nothing
convertInline n cm (R.CommentLink mt c mlabel) = do
  let mticketN = mt <|> n
      mlabelInline = fmap (map Str) mlabel
  case mticketN of
    Nothing ->
      traceShow
        ("NO TICKET NUMBER", n, mt, c)
        (pure . Str $ fromMaybe "?" (unwords <$> mlabel))
    Just ticketN ->
      cm ticketN c >>= \case
        (NoteRef t) ->
            pure (TicketLink mlabelInline ticketN (Just t))
        (CommitRef hash mrepo) ->
          pure $ GitCommitLink (fromMaybe (prettyCommit hash mrepo) mlabelInline) hash mrepo
        MissingCommentRef ->
          traceShow
            ("COULD NOT FIND", n, mt, c)
            (pure $ TicketLink mlabelInline ticketN Nothing)
convertInline _ _ e = error $ "not handled"  ++ show e
