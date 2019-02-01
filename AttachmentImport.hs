{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AttachmentImport where

import Control.Monad
import Control.Monad.Catch hiding (bracket, onError)
import Control.Monad.Error.Class
import Control.Monad.IO.Class

import qualified Data.Set as S
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Logging
import Database.PostgreSQL.Simple
import qualified Trac.Web
import Trac.Db as Trac
import Trac.Db.Types as Trac
import GitLab.Common
import GitLab.Tickets
import GitLab.UploadFile
import GitLab.Project
import UserLookup
import Servant.Client
import ImportState
import Settings

makeAttachments :: Logger -> Connection -> UserIdOracle -> ClientM ()
makeAttachments logger conn userIdOracle = do
    attachments <- liftIO $ getAttachments conn
    (finishedAttachments, finishAttachment) <-
        liftIO $ openStateFile attachmentStateFile
    let makeAttachment' a
          | aIdent `S.member` finishedAttachments = return ()
          | otherwise = handleAll onError $ flip catchError onError $ do
            makeAttachment logger userIdOracle a
            liftIO $ finishAttachment aIdent
          where
            aIdent = (aResource a, aFilename a, aTime a)
            onError :: (MonadIO m, Show a) => a -> m ()
            onError err =
                liftIO $ writeLog logger "ERROR" $ "Failed to create attachment " ++ show a ++ ": " ++ show err
    mapM_ makeAttachment' $ attachments

makeAttachment :: Logger -> UserIdOracle -> Attachment -> ClientM ()
makeAttachment logger userIdOracle (Attachment{..})
  | TicketAttachment ticketNum <- aResource = do
        liftIO $ writeLog logger "ATTACHMENT" $ show (aResource, aFilename)
        content <- liftIO $ Trac.Web.fetchTicketAttachment tracBaseUrl ticketNum aFilename
        uid <- findOrCreateUser userIdOracle aAuthor
        msg <- if ".hs" `T.isSuffixOf` aFilename && BS.length content < 30000
            then mkSnippet uid ticketNum content
            else mkAttachment uid ticketNum content
        mkComment uid (IssueIid $ fromIntegral $ getTicketNumber ticketNum) msg
  | otherwise = return ()
  where
    mkSnippet, mkAttachment :: UserId -> TicketNumber -> BS.ByteString -> ClientM Text
    mkSnippet uid ticketNum content = do
        let title = T.unwords [ aFilename, "from ticket"
                              , "#" <> T.pack (show $ getTicketNumber ticketNum)
                              ]
            cs = CreateSnippet { csTitle = title
                               , csFileName = aFilename
                               , csDescription = Just $ T.unlines [ aDescription
                                                                  , ""
                                                                  , title
                                                                  ]
                               , csCode = TE.decodeUtf8 content
                               , csVisibility = Public
                               }
        sid <- GitLab.Project.createSnippet gitlabToken (Just uid) project cs
        return $ T.unlines
            [ "Attached file `" <> aFilename <> "` ($" <> T.pack (show $ getSnippetId sid) <> ")."
            , ""
            , aDescription
            ]
    mkAttachment uid _ticketNum content = do
        url <- GitLab.UploadFile.uploadFile gitlabToken (Just uid) project aFilename content
        return $ T.unlines
            [ "Attached file `" <> aFilename <> "` ([download](" <> url <> "))."
            , ""
            , aDescription
            ]

    mkComment :: UserId -> IssueIid -> Text -> ClientM ()
    mkComment uid iid msg = do
        let note = CreateIssueNote { cinBody = msg
                                   , cinCreatedAt = Just aTime
                                   }
        void $ createIssueNote gitlabToken (Just uid) project iid note

