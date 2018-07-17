module FOMObot.App
    ( initApp
    ) where

import qualified Data.Text as T
import System.Environment (getEnv)
import Control.Lens (uses, views, (^.))
import Control.Monad (void, when)
import qualified Web.Slack as Slack

import FOMObot.Helpers.CommandProcessor
import FOMObot.Helpers.DMChannel
import FOMObot.Helpers.FOMOChannel
import FOMObot.Helpers.MessageProcessor
import FOMObot.Helpers.Preferences
import FOMObot.Types.AppState
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.BotState

runApp :: Slack.Event -> Bot ()
runApp m@(Slack.Message cid (Slack.UserComment uid) _ _ _ _) = do
    ignoreFOMOChannel <- isFOMOChannel cid
    isDM <- isDMChannel uid cid

    case () of
      _ | ignoreFOMOChannel -> return ()
        | isDM -> processCommand m
        | otherwise -> do
            eventOccurred <- processMessage m
            when eventOccurred $ do
                alertUsers cid
                alertFOMOChannel cid

runApp (Slack.ImCreated uid (Slack.IM cid _ _ _ _ _)) = setDMChannel uid cid

runApp Slack.Hello = do
    ims <- uses Slack.session (views Slack.slackIms $ map pullOutUserAndChannel)
    mapM_ (uncurry setDMChannel) ims
  where
    pullOutUserAndChannel im = (im ^. Slack.imUser, im ^. Slack.imId)

runApp _ = return ()

initApp :: IO ()
initApp = do
    token <- getEnv "SLACK_API_TOKEN"
    config <- buildConfig
    void $ Slack.runBot (Slack.SlackConfig token) runApp $ AppState config emptyState
