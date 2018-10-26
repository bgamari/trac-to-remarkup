{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Database.PostgreSQL.Simple
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client

import GitLab.Tickets
import Trac.Db
import Trac.Db.Types

main :: IO ()
main = do
    conn <- connectPostgreSQL ""
    tickets <- getTickets conn
    --mapM_ print tickets
    mapM_ (getTicketChanges conn >=> print) $ map ticketNumber tickets

    mgr <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv mgr (BaseUrl Https "localhost" 80 "")
    res <- runClientM (run $ head tickets) clientEnv
    return ()

run :: Ticket -> ClientM ()
run t = do
    let description = T.unlines
            [ fieldsTable fields
            , ""
            , runIdentity $ ticketDescription (ticketFields t)
            ]
        fields = ticketFields t
        issue = CreateIssue { ciIid = Just $ case ticketNumber t of
                                               TicketNumber n -> IssueIid $ fromIntegral n
                            , ciTitle = runIdentity $ ticketSummary fields
                            , ciLabels = Just $ fieldLabels $ hoistFields (Just . runIdentity) fields
                            , ciCreatedAt = Just $ ticketCreationTime t
                            , ciDescription = Just description
                            }
    ir <- createIssue (ProjectId 40) issue
    return ()

-- | Maps Trac keywords to labels
keywordLabels :: M.Map Text Labels
keywordLabels = M.fromList
    $ [ passthru "newcomer"
      , ("newcomers", "newcomer")
      , passthru "TypeInType"
      , passthru "TypeFamilies"
      , passthru "PatternSynonyms"
      , passthru "Deriving"
      , passthru "Generics"
      , passthru "PatternMatchWarnings"
      , passthru "Inlining"
      , passthru "QuantifiedConstraints"
      , passthru "TypeApplications"
      , passthru "LevityPolymorphism"
      , passthru "CodeGen"
      , passthru "GADTs"
      , passthru "JoinPoints"
      , passthru "Typeable"
      , ("Typeable", "typeable")
      , ("ORF", "OverloadedRecordFields")
      , ("hs-boot", "hs-boot")
      , passthru "SpecConstr"
      , passthru "ApplicativeDo"
      , passthru "FunDeps"
      , passthru "TypedHoles"
      , passthru "CSE"
      , ("TypeCheckerPlugins", "Plugins")
      , ("deriving-perf", "Deriving" <> "CompilerPerformance")
      , passthru "CUSKs"
      , passthru "PolyKinds"
      , ("performance", "RuntimePerformance")
      , ("ci-breakage", "CI-Breakage")

      , ("DWARF", "DebugInformation")
      , passthru "SafeHaskell"
      , passthru "CustomTypeErrors"
      , passthru "StaticPointers"
      , passthru "Unicode"
      , ("warnings", "ErrorMessages")
      , passthru "Arrows"
      , passthru "SIMD"
      , passthru "TemplateHaskell"
      ]
  where
    passthru x = (x, mkLabel x)

typeOfFailureLabels :: TypeOfFailure -> Labels
typeOfFailureLabels t =
    case t of
      BuildingGhcFailed         -> "GhcBuildFailure"
      CompileTimeCrash          -> "CompilerCrash"
      CompileTimePerformance    -> "CompilerPerformance"
      IncorrectDebugInformation -> "DebugInformation"
      DocumentationBug          -> "Documentation"
      InvalidProgramAccepted    -> mempty
      GhcDoesn'tWork            -> mempty
      GhciCrash                 -> "GHCi"
      ValidProgramRejected      -> mempty
      IncorrectAPIAnnotation    -> "APIAnnotations"
      PoorErrorMessage          -> "ErrorMessages"
      IncorrectWarning          -> "ErrorMessages"
      IncorrectResultAtRuntime  -> "IncorrectResultAtRuntime"
      InstallationFailure       -> mempty
      RuntimeCrash              -> "RuntimeCrash"
      RuntimePerformance        -> "RuntimePerformance"
      OtherFailure              -> mempty

fieldLabels :: Fields Maybe -> Labels
fieldLabels fields =
    keywordLbls <> failureLbls
  where
    keywordLbls = mconcat
        [ lbl
        | Just keywords <- pure $ ticketKeywords fields
        , kw <- keywords
        , Just lbl <- pure $ M.lookup kw keywordLabels
        ]

    failureLbls :: Labels
    failureLbls = maybe mempty typeOfFailureLabels $ ticketTypeOfFailure fields

fieldsTable :: forall f. (Functor f, Foldable f) => Fields f -> T.Text
fieldsTable (Fields{..}) =
    T.unlines
    [ renderRow header
    , renderRow (T.replicate (fst widths) "-", T.replicate (snd widths) "-")
    ]
  where
    row :: Text -> f Text -> Maybe (Text, Text)
    row name val =
        listToMaybe $ toList $ fmap (\v -> (name,v)) val

    rows :: [(Text, Text)]
    rows =
        catMaybes
        [ row "version" ticketVersion
        , row "test case" ticketTestCase
        , row "differential revisions" $ fmap renderTicketDifferentials ticketDifferentials
        ]

    header :: (Text, Text)
    header = ("field", "value")

    widths = ( maximum $ map (T.length . fst) (header:rows)
             , maximum $ map (T.length . snd) (header:rows)
             )

    renderRow :: (Text, Text) -> Text
    renderRow (a,b) = mconcat
        [ "| "
        , T.justifyLeft (fst widths) ' ' a
        , " | "
        , T.justifyLeft (snd widths) ' ' b
        , " |"
        ]

renderTicketDifferentials :: [Differential] -> Text
renderTicketDifferentials _ = ""

toPriorityLabel :: Priority -> Labels
toPriorityLabel p = case p of
  PrioLowest  -> "P-lowest"
  PrioLow     -> "P-low"
  PrioNormal  -> "P-normal"
  PrioHigh    -> "P-high"
  PrioHighest -> "P-highest"
