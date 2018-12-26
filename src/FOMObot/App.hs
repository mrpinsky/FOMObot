module FOMObot.App
    ( initApp
    ) where

import System.Environment (getEnv)
import Network.Wreq as Wreq (Response, Options, postWith, defaults, header, responseBody, FormParam((:=)))

import Control.Lens (preview, (.~), (&))
import Control.Monad (unless, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)

import Data.Aeson.Lens
import Data.ByteString.Lazy.Internal
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Monoid ((<>))

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
    let config = Slack.SlackConfig{ Slack._slackApiToken = token }
    appState <- AppState.init <$> buildConfig
    Slack.withSlackHandle config $ \h -> evalStateT (runBot $ fomo h) appState

fomo :: Slack.SlackHandle -> Bot ()
fomo handle =
    forever $ liftIO (Slack.getNextEvent handle) >>= runApp handle


runApp :: Slack.SlackHandle -> Slack.Event -> Bot ()
runApp handle m@(Slack.Message cid (Slack.UserComment uid) _ _ _ _) =
    unless (isFOMOChannel handle cid) $ isDMChannel uid cid >>= processEvent
  where
    processEvent :: Bool -> Bot ()
    processEvent isDM =
        if isDM then
            processCommand handle m
        else
            processMessage m
            >>= handleProcessedMessage

    handleProcessedMessage :: Maybe T.Text -> Bot ()
    handleProcessedMessage Nothing = return ()
    handleProcessedMessage (Just eventText) =
        logEventText eventText
        >> extractTopic eventText
        >>= sendAlerts

    logEventText :: T.Text -> Bot ()
    logEventText text =
        botLog $ T.intercalate " "
            [ "Channel"
            , Slack._getId cid
            , "extracting topic from"
            , text
            ]

    sendAlerts :: Maybe T.Text -> Bot ()
    sendAlerts topic =
        alertUsers handle topic cid
        >> alertFOMOChannel handle topic cid
        >> logAlerts topic

    logAlerts :: Maybe T.Text -> Bot()
    logAlerts topic =
        botLog $ T.intercalate " "
            [ "Channel"
            , Slack._getId cid
            , "alerted users"
            , loggableAlertText topic
            ]

    loggableAlertText :: Maybe T.Text -> T.Text
    loggableAlertText Nothing = "without a topic"
    loggableAlertText (Just topic) = "with topic: " <> topic


runApp handle (Slack.ImCreated uid (Slack.IM cid _ _ _ _ _)) = setDMChannel uid cid

-- runApp handle Slack.Hello =
--     Slack.getSession handle $ views Slack.slackIms $ List.map pullOutUserAndChannel
--     >>= mapM_ (uncurry setDMChannel)
--   where
--     pullOutUserAndChannel im = (im ^. Slack.imUser, im ^. Slack.imId)

runApp _ _ = return ()

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
