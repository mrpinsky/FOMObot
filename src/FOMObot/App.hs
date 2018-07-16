module FOMObot.App
    ( initApp
    ) where

import System.Environment (getEnv)
import Control.Lens (uses, views, (^.))
import Control.Monad (when, unless)
import qualified Web.Slack as Slack

import FOMObot.Helpers.CommandProcessor
import FOMObot.Helpers.DMChannel
import FOMObot.Helpers.FOMOChannel
import FOMObot.Helpers.MessageProcessor
import qualified FOMObot.Types.AppState as AppState
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig

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

        handleProcessedMessage :: Bool -> Bot ()
        handleProcessedMessage eventOccurred =
            when eventOccurred $
                alertUsers cid
                >> alertFOMOChannel cid

runApp (Slack.ImCreated uid (Slack.IM cid _ _ _ _ _)) = setDMChannel uid cid

runApp Slack.Hello =
    uses Slack.session (views Slack.slackIms $ map pullOutUserAndChannel)
    >>= mapM_ (uncurry setDMChannel)
  where
    pullOutUserAndChannel im = (im ^. Slack.imUser, im ^. Slack.imId)

runApp _ = return ()

initApp :: IO ()
initApp = do
    token <- getEnv "SLACK_API_TOKEN"
    config <- buildConfig
    Slack.runBot (Slack.SlackConfig token) runApp $ AppState.init config
