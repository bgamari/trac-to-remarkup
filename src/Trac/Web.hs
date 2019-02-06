{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Trac.Web where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Text (Text)
import Data.Semigroup
import Data.Maybe
import Trac.Db.Types
import Servant.Client
import System.Process
import System.Exit

----------------------------------------------------------------------
-- fetchTicketAttachment
----------------------------------------------------------------------

fetchTicketAttachment :: BaseUrl -> TicketNumber -> Text -> IO ByteString
fetchTicketAttachment baseUrl (TicketNumber n) filename = do
    let url = intercalate "/"
              [ showBaseUrl baseUrl
              , "raw-attachment/ticket"
              , show n
              , T.unpack filename
              ]
        cp = (proc "curl" ["--silent", url]) { std_out = CreatePipe }
    (_, Just out, _, h) <- createProcess cp
    content <- BS.hGetContents out
    exit <- waitForProcess h
    case exit of
      ExitFailure n -> fail $ "Fetching "++show url++" failed with code "++show n
      ExitSuccess -> return ()
    return content

