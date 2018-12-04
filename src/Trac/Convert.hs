{-#LANGUAGE LambdaCase #-}

module Trac.Convert (convert, CommentMap, LookupComment) where

import qualified Trac.Parser as R
import Trac.Writer
import Data.List
import qualified Data.Map as M
import Debug.Trace
import Data.Maybe

type LookupComment = Int -> Int -> IO (Maybe Int)

convert :: String -> String -> String -> Int -> LookupComment -> String -> IO String
convert base org proj n cm s =
    fmap (writeRemarkup base org proj)
    $ convertBlocks n cm
    $ either (\err -> [R.Para [R.Str "NO PARSE: ", R.Str $ show err, R.Str s]]) id
    $ R.parseTrac
    $ s

-- convertWithError org proj n cm s = writeRemarkup org proj . convertBlocks n cm <$> R.parseTrac s

convertBlocks :: Int -> LookupComment -> [R.Block] -> IO [Block]
convertBlocks n cm = mapM (convertBlock n cm)

convertBlock :: Int -> LookupComment -> R.Block -> IO Block
convertBlock n cm (R.Header nlev is bs)
  = Header nlev <$> convertInlines n cm is <*> convertBlocks n cm bs
convertBlock n cm (R.Para is)        = Para <$> convertInlines n cm is
convertBlock n cm (R.List _ bs)      = List Style . map (:[]) <$> convertBlocks n cm bs
convertBlock n cm (R.DefnList d)     = convertDefnListToTable n cm d
convertBlock n cm (R.Code ty s)      = pure $ CodeBlock ty s
convertBlock n cm (R.BlockQuote bs)  = Quote . (:[]) . Para <$> convertInlines n cm bs
convertBlock n cm (R.Discussion bs)  = Quote <$> convertBlocks n cm bs
convertBlock n cm (R.Table rs)       = Table <$> convertTableRows n cm rs
convertBlock _ _ R.HorizontalLine    = pure HorizontalLine

convertTableRows :: Int -> LookupComment -> [R.TableRow] -> IO [TableRow]
convertTableRows n cm = mapM (convertTableRow n cm)

convertTableRow :: Int -> LookupComment -> R.TableRow -> IO TableRow
convertTableRow n cm = mapM (convertTableCell n cm)

convertTableCell :: Int -> LookupComment -> R.TableCell -> IO TableCell
convertTableCell n cm (R.TableHeaderCell is) = TableHeaderCell <$> convertInlines n cm is
convertTableCell n cm (R.TableCell is) = TableCell <$> convertInlines n cm is

convertDefnListToTable :: Int -> LookupComment -> [(R.Inlines, [R.Inlines])] -> IO Block
convertDefnListToTable n cm [] = pure $ Para []
convertDefnListToTable n cm items = Table . mconcat <$> mapM (convertDefnToTableRows n cm) items

convertDefnToTableRows :: Int -> LookupComment -> (R.Inlines, [R.Inlines]) -> IO [TableRow]
convertDefnToTableRows n cm (dh, []) = (:[]) . (:[]) . TableHeaderCell <$> convertInlines n cm dh
convertDefnToTableRows n cm (dh, dd:dds) = (:) <$> convertFirstDefnRow n cm dh dd <*> convertAdditionalDefnRows n cm dds

convertFirstDefnRow :: Int -> LookupComment -> R.Inlines -> R.Inlines -> IO TableRow
convertFirstDefnRow n cm dh dd =
  sequence
    [ TableHeaderCell <$> convertInlines n cm dh
    , TableCell <$> convertInlines n cm dd
    ]

convertAdditionalDefnRow :: Int -> LookupComment -> R.Inlines -> IO TableRow
convertAdditionalDefnRow n cm dd =
  sequence
    [ pure $ TableCell []
    , TableCell <$> convertInlines n cm dd
    ]

convertAdditionalDefnRows :: Int -> LookupComment -> [R.Inlines] -> IO [TableRow]
convertAdditionalDefnRows n cm = mapM (convertAdditionalDefnRow n cm)

convertInlines n cm = mapM (convertInline n cm)

convertInline :: Int -> LookupComment -> R.Inline -> IO Inline
convertInline n cm (R.Bold is) = Bold <$> convertInlines n cm is
convertInline n cm (R.Monospaced ty is) = pure (Monospaced ty is)
convertInline n cm (R.Italic is) = Italic <$> convertInlines n cm is
convertInline n cm (R.WikiStyle is) = Italic <$> convertInlines n cm is
convertInline n cm (R.Link url []) = pure $ WebLink (intersperse Space [Str url]) url
convertInline n cm (R.Link url is) = pure $ WebLink (intersperse Space (map Str is)) url
convertInline n cm (R.GitCommitLink hash []) = pure $ GitCommitLink (intersperse Space [Str hash]) hash
convertInline n cm (R.GitCommitLink hash is) = pure $ GitCommitLink (intersperse Space (map Str is)) hash
convertInline n cm (R.Str s) = pure $ Str s
convertInline _ _ (R.LineBreak)  = pure LineBreak
convertInline _ _ (R.Space)      = pure Space
convertInline _ _ (R.DifferentialLink n) =
  pure $ DifferentialLink n
convertInline _ _ (R.TracTicketLink n desc) =
  pure $ TicketLink (fmap (map Str) desc) n Nothing
convertInline n cm (R.CommentLink mt c mlabel) = do
  let ticketN = fromMaybe n mt
      mlabelInline = fmap (map Str) mlabel
  cm ticketN c >>= \case
      Just t ->
        pure (TicketLink mlabelInline ticketN (Just t))
      Nothing ->
        traceShow
          ("COULD NOT FIND", n, mt, c)
          (pure $ TicketLink mlabelInline ticketN Nothing)
convertInline _ _ e = error $ "not handled"  ++ show e
