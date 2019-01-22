{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}

module Trac.Convert (convert, convertIgnoreErrors, convertBlocks, CommentMap, LookupComment, runConvert) where

import qualified Trac.Parser as R
import Trac.Writer
import Trac.Db.Types
import Data.List
import qualified Data.Map as M
import Debug.Trace
import Data.Maybe
import Control.Applicative
import Control.Exception
import Text.Megaparsec.Error (ParseError, parseErrorPretty)
import Data.Void
import Text.Printf
import Logging
import Control.Monad.Reader

type LookupComment = Int -> Int -> IO CommentRef

data ConvertContext
  = ConvertContext
      { logger :: Logger
      , mTicketNumber :: Maybe Int
      , lookupComment :: LookupComment
      }

type Convert = ReaderT ConvertContext IO

instance MonadLogger (ReaderT ConvertContext IO) where
  getLogger = liftLogger <$> asks logger

runConvert :: Logger -> Maybe Int -> LookupComment -> Convert a -> IO a
runConvert logger mn cm action =
  runReaderT action (ConvertContext logger mn cm)

convert :: Logger
        -> String
        -> String
        -> String
        -> Maybe Int
        -> Maybe String
        -> LookupComment
        -> String
        -> IO String
convert logger base org proj mn msrcname cm s =
    fmap (writeRemarkup base org proj)
    $ runConvert logger mn cm
    $ convertBlocks
    $ either throw id
    $ R.parseTrac (msrcname <|> (("ticket:" ++) . show <$> mn))
    $ s

convertIgnoreErrors :: Logger -> String -> String -> String -> Maybe Int -> Maybe String -> LookupComment -> String -> IO String
convertIgnoreErrors logger base org proj mn msrcname cm s =
  convert logger base org proj mn msrcname cm s
    `catches`
      [ Handler handleParseError
      , Handler handleOtherError
      ]
  where
    handleOtherError :: SomeException -> IO String
    handleOtherError err = do
      writeLog logger "CONVERSION-ERROR" $ displayException err
      return $
        printf "Error:\n\n```\n%s\n```\n\nOriginal Trac source:\n\n```trac\n%s\n```\n"
          (displayException err) s

    handleParseError :: ParseError Char Void -> IO String
    handleParseError err = do
      writeLog logger "PARSER-ERROR" $ parseErrorPretty err
      fmap (writeRemarkup base org proj) $
        runConvert logger mn cm $
          convertBlocks
            [R.Header 1
              [R.Str "PARSER ERROR:"]
              [ R.Para [R.Str $ parseErrorPretty err]
              , R.Code (Just "trac") s
              ]
            ]


-- (\err -> [R.Para [R.Str "NO PARSE: ", R.Str $ show err, R.Str s]])

-- convertWithError org proj n cm s = writeRemarkup org proj . convertBlocks n cm <$> R.parseTrac s

convertBlocks :: [R.Block] -> Convert [Block]
convertBlocks = mapM convertBlock

convertBlock :: R.Block -> Convert Block
convertBlock (R.Header nlev is bs)
  = Header nlev <$> convertInlines is <*> convertBlocks bs
convertBlock (R.Para is)        = Para <$> convertInlines is
convertBlock (R.List R.BulletListType bs) =
  List BulletStyle <$> mapM convertBlocks bs
convertBlock (R.List R.NumberedListType bs) =
  List NumberedStyle <$> mapM convertBlocks bs
convertBlock (R.DefnList d)     = convertDefnListToTable d
convertBlock (R.Code ty s)      = pure $ CodeBlock ty s
convertBlock (R.BlockQuote bs)  = Quote . (:[]) . Para <$> convertInlines bs
convertBlock (R.Discussion bs)  = Quote <$> convertBlocks bs
convertBlock (R.Table rs)       = Table <$> convertTableRows rs
convertBlock R.HorizontalLine   = pure HorizontalLine

convertTableRows :: [R.TableRow] -> Convert [TableRow]
convertTableRows = mapM convertTableRow

convertTableRow :: R.TableRow -> Convert TableRow
convertTableRow = mapM convertTableCell

convertTableCell :: R.TableCell -> Convert TableCell
convertTableCell (R.TableHeaderCell is) = TableHeaderCell <$> convertBlocks is
convertTableCell (R.TableCell is) = TableCell <$> convertBlocks is

convertDefnListToTable :: [(R.Blocks, [R.Blocks])] -> Convert Block
convertDefnListToTable [] = pure $ Para []
convertDefnListToTable items = Table . mconcat <$> mapM convertDefnToTableRows items

convertDefnToTableRows :: (R.Blocks, [R.Blocks]) -> Convert [TableRow]
convertDefnToTableRows (dh, []) = (:[]) . (:[]) . TableHeaderCell <$> convertBlocks dh
convertDefnToTableRows (dh, dd:dds) = (:) <$> convertFirstDefnRow dh dd <*> convertAdditionalDefnRows dds

convertFirstDefnRow :: R.Blocks -> R.Blocks -> Convert TableRow
convertFirstDefnRow dh dd =
  sequence
    [ TableHeaderCell <$> convertBlocks dh
    , TableCell <$> convertBlocks dd
    ]

convertAdditionalDefnRow :: R.Blocks -> Convert TableRow
convertAdditionalDefnRow dd =
  sequence
    [ pure $ TableCell []
    , TableCell <$> convertBlocks dd
    ]

convertAdditionalDefnRows :: [R.Blocks] -> Convert [TableRow]
convertAdditionalDefnRows = mapM convertAdditionalDefnRow

convertInlines = mapM convertInline

prettyCommit :: CommitHash -> Maybe RepoName -> [Inline]
prettyCommit hash Nothing = [Str $ take 7 hash]
prettyCommit hash (Just repo) = [Str repo, Str ":", Str $ take 7 hash]

convertInline :: R.Inline -> Convert Inline
convertInline (R.Bold is) = Bold <$> convertInlines is
convertInline (R.Monospaced ty is) = pure (Monospaced ty is)
convertInline (R.Italic is) = Italic <$> convertInlines is
convertInline (R.WikiStyle is) = Italic <$> convertInlines is
convertInline (R.Link url []) = pure $ WebLink (intersperse Space [Str url]) url
convertInline (R.Link url is) = pure $ WebLink (intersperse Space (map Str is)) url
convertInline (R.WikiLink wikiname mlabel) = do
  let url = tracWikiNameToGitlab wikiname
      label = map Str $ fromMaybe [wikiname] mlabel
  pure $ WikiLink label url
convertInline (R.GitCommitLink hash mrepo []) = pure $ GitCommitLink (intersperse Space (prettyCommit hash mrepo)) hash mrepo
convertInline (R.GitCommitLink hash mrepo is) = pure $ GitCommitLink (intersperse Space (map Str is)) hash mrepo
convertInline (R.Str s) = pure $ Str s
convertInline (R.LineBreak)  = pure LineBreak
convertInline (R.Space)      = pure Space
convertInline (R.DifferentialLink n) =
  pure $ DifferentialLink n
convertInline (R.TracTicketLink n desc) =
  pure $ TicketLink (fmap (map Str) desc) n Nothing
convertInline (R.CommentLink mt c mlabel) = do
  n <- asks mTicketNumber
  let mticketN = mt <|> n
  let mlabelInline = fmap (map Str) mlabel
  case mticketN of
    Nothing -> do
      writeLogM "CONVERT-WARNING" $
        printf "No ticket number (%s, %s, %s)"
          (show n)
          (show mt)
          (show c)
      pure . Str $ fromMaybe "?" (unwords <$> mlabel)
    Just ticketN -> do
      cm <- asks lookupComment
      liftIO (cm ticketN c) >>= \case
        (NoteRef t) ->
            pure (TicketLink mlabelInline ticketN (Just t))
        (CommitRef hash mrepo) ->
          pure $ GitCommitLink (fromMaybe (prettyCommit hash mrepo) mlabelInline) hash mrepo
        MissingCommentRef -> do
          writeLogM "CONVERT-WARNING" $
            printf "Could not find comment (%s, %s, %s)"
              (show n)
              (show mt)
              (show c)
          pure $ TicketLink mlabelInline ticketN Nothing
convertInline (R.Small is) = Small <$> convertInlines is
convertInline (R.Superscript is) = Superscript <$> convertInlines is
convertInline (R.Subscript is) = Subscript <$> convertInlines is
convertInline (R.Strikethrough is) = Deleted <$> convertInlines is
convertInline e = do
  writeLogM "CONVERT-WARNING" $ "not handled: " ++ show e
  pure $ Str $ "not handled: "  ++ show e
