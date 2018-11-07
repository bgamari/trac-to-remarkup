module Trac.Convert (convert, CommentMap) where

import qualified Trac.Parser as R
import Trac.Writer
import Data.List
import qualified Data.Map as M
import Debug.Trace
import Data.Maybe


convert :: Int -> CommentMap -> String -> String
convert n cm s =
    writeRemarkup
    $ convertBlocks n cm
    $ either (\err -> [R.Para [R.Str "NO PARSE: ", R.Str $ show err, R.Str s]]) id
    $ R.parseTrac
    $ s

convertWithError n cm s = writeRemarkup . convertBlocks n cm <$> R.parseTrac s

convertBlocks :: Int -> CommentMap -> [R.Block] -> [Block]
convertBlocks n cm = map (convertBlock n cm)

convertBlock :: Int -> CommentMap -> R.Block -> Block
convertBlock n cm (R.Header nlev is bs)
  = Header nlev (convertInlines n cm is) (convertBlocks n cm bs)
convertBlock n cm (R.Para is)        = Para (convertInlines n cm is)
convertBlock n cm (R.List _ bs)      = List Style (map (:[]) (convertBlocks n cm bs))
convertBlock n cm (R.DefnList d)     = convertDefnListToTable n cm d
convertBlock n cm (R.Code ty s)      = CodeBlock ty s
convertBlock n cm (R.BlockQuote bs)  = Quote [Para (convertInlines n cm bs)]
convertBlock n cm (R.Discussion bs)  = Quote (convertBlocks n cm bs)
convertBlock n cm (R.Table rs)       = Table (convertTableRows n cm rs)
convertBlock _ _ R.HorizontalLine    = HorizontalLine

convertTableRows :: Int -> CommentMap -> [R.TableRow] -> [TableRow]
convertTableRows n cm = map (convertTableRow n cm)

convertTableRow :: Int -> CommentMap -> R.TableRow -> TableRow
convertTableRow n cm = map (convertTableCell n cm)

convertTableCell :: Int -> CommentMap -> R.TableCell -> TableCell
convertTableCell n cm (R.TableHeaderCell is) = TableHeaderCell (convertInlines n cm is)
convertTableCell n cm (R.TableCell is) = TableCell (convertInlines n cm is)

convertDefnListToTable :: Int -> CommentMap -> [(R.Inlines, [R.Inlines])] -> Block
convertDefnListToTable n cm [] = Para []
convertDefnListToTable n cm items = Table $ mconcat (map (convertDefnToTableRows n cm) items)

convertDefnToTableRows :: Int -> CommentMap -> (R.Inlines, [R.Inlines]) -> [TableRow]
convertDefnToTableRows n cm (dh, []) = [[ TableHeaderCell $ convertInlines n cm dh ]]
convertDefnToTableRows n cm (dh, dd:dds) = convertFirstDefnRow n cm dh dd : convertAdditionalDefnRows n cm dds

convertFirstDefnRow :: Int -> CommentMap -> R.Inlines -> R.Inlines -> TableRow
convertFirstDefnRow n cm dh dd = [ TableHeaderCell $ convertInlines n cm dh, TableCell $ convertInlines n cm dd ]

convertAdditionalDefnRow :: Int -> CommentMap -> R.Inlines -> TableRow
convertAdditionalDefnRow n cm dd = [ TableCell [], TableCell $ convertInlines n cm dd ]

convertAdditionalDefnRows :: Int -> CommentMap -> [R.Inlines] -> [TableRow]
convertAdditionalDefnRows n cm = map (convertAdditionalDefnRow n cm)

convertInlines n cm = map (convertInline n cm)

convertInline :: Int -> CommentMap -> R.Inline -> Inline
convertInline n cm (R.Bold is) = Bold (convertInlines n cm is)
convertInline n cm (R.Monospaced is) = Monospaced is
convertInline n cm (R.Italic is) = Italic (convertInlines n cm is)
convertInline n cm (R.WikiStyle is) = Italic (convertInlines n cm is)
convertInline n cm (R.Link url is) = WebLink (intersperse Space (map Str is)) url
convertInline n cm (R.Str s) = Str s
convertInline _ _ (R.LineBreak)  = LineBreak
convertInline _ _ (R.Space)      = Space
convertInline _ _ (R.TracTicketLink n) = TicketLink n Nothing
convertInline n cm (R.CommentLink mt c) =
  let ticketN = fromMaybe n mt
  in case M.lookup (ticketN, c) cm of
      Just t -> TicketLink ticketN (Just t)
      Nothing -> traceShow ("COULD NOT FIND", n, mt, c, cm) (TicketLink ticketN Nothing)
convertInline _ _ e = error $ "not handled"  ++ show e
