module FOMObot.App
    ( initApp
    ) where

import Data.Aeson.Lens
import Data.ByteString.Lazy.Internal
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Environment (getEnv)
import Control.Lens (uses, views, preview, (^.), (.~), (&))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Network.Wreq as Wreq (Response, Options, postWith, defaults, header, responseBody, FormParam((:=)))
import qualified Web.Slack as Slack

import FOMObot.Helpers.CommandProcessor
import FOMObot.Helpers.DMChannel
import FOMObot.Helpers.FOMOChannel
import FOMObot.Helpers.MessageProcessor
import qualified FOMObot.Types.AppState as AppState
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig

initApp :: IO ()
initApp = do
    token <- getEnv "SLACK_API_TOKEN"
    config <- buildConfig
    Slack.runBot (Slack.SlackConfig token) runApp $ AppState.init config

runApp :: Slack.Event -> Bot ()
runApp m@(Slack.Message cid (Slack.UserComment uid) _ _ _ _) = isFOMOChannel cid >>= handleEvent
    where
        handleEvent :: Bool -> Bot ()
        handleEvent ignoreFOMOChannel =
            unless ignoreFOMOChannel $
                isDMChannel uid cid
                >>= processEvent

        processEvent :: Bool -> Bot ()
        processEvent isDM =
            if isDM then
                processCommand m
            else
                processMessage m
                >>= handleProcessedMessage

        handleProcessedMessage :: Maybe T.Text -> Bot ()
        handleProcessedMessage Nothing = return ()
        handleProcessedMessage (Just eventText) =
            extractTopic eventText
            >>= sendAlerts

        sendAlerts :: Maybe T.Text -> Bot ()
        sendAlerts topic =
            alertUsers topic cid
            >> alertFOMOChannel topic cid


runApp (Slack.ImCreated uid (Slack.IM cid _ _ _ _ _)) = setDMChannel uid cid

runApp Slack.Hello =
    uses Slack.session (views Slack.slackIms $ List.map pullOutUserAndChannel)
    >>= mapM_ (uncurry setDMChannel)
  where
    pullOutUserAndChannel im = (im ^. Slack.imUser, im ^. Slack.imId)

runApp _ = return ()

extractTopic :: T.Text -> Bot (Maybe T.Text)
extractTopic eventText =
    parseResponse <$> postToApi eventText

postToApi :: T.Text -> Bot (Wreq.Response ByteString)
postToApi topic = liftIO $ makeRequest =<< reqOpts
  where
    makeRequest :: Wreq.Options -> IO (Wreq.Response ByteString)
    makeRequest opts = postWith opts url body

    reqOpts :: IO Wreq.Options
    reqOpts = do
        appKey <- E.encodeUtf8 . T.pack <$> getEnv "AYLIEN_APP_KEY"
        appId <- E.encodeUtf8 . T.pack <$> getEnv "AYLIEN_APP_ID"

        return $ defaults
            & header "X-AYLIEN-TextAPI-Application-Key" .~ [appKey]
            & header "X-AYLIEN-TextAPI-Application-ID" .~ [appId]

    url :: String
    url = "https://api.aylien.com/api/v1/classify"

    body :: [Wreq.FormParam]
    body = ["text" := topic]

parseResponse :: Response ByteString -> Maybe T.Text
parseResponse response = mungeSpecficLabel <$> preview label response
  where
    mungeSpecficLabel :: T.Text -> T.Text
    mungeSpecficLabel = List.last . T.splitOn " - "

    label = responseBody . key "categories" . nth 0 . key "label" . _String
