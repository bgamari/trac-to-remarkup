{-# LANGUAGE QuasiQuotes #-}

module TicketFixup where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Logging
import Trac.Db
import Trac.Db.Types
import GitLab.Common (ProjectId(..))
import Settings

-- | Make the last-updated time of GitLab tickets match those of Trac.
fixupLastUpdated :: Logger -> IO ()
fixupLastUpdated logger = do
    gitlabConn <- connectPostgreSQL gitlabDsn
    tracConn <- connectPostgreSQL tracDsn
    tickets <- getTickets tracConn
    updated <- executeMany gitlabConn 
      [sql|UPDATE issues 
           SET issues.updated_at = upd.time
           FROM (VALUES (?,?,?)) AS upd(project,ticket,time)
           WHERE issues.iid = upd.ticket
             AND issues.project_id = upd.project
          |]
      [ (projId, ticketNumber t, ticketChangeTime t)
      | t <- tickets
      ]
    writeLog logger "FIXED UP" $ show updated ++ " tickets"
    return ()
  where
    ProjectId projId = project
