module FOMObot.Helpers.CommandProcessor
    ( processCommand
    ) where

import Control.Lens (view)
import Control.Monad.IO.Class (liftIO)

import Data.Monoid ((<>))
import qualified Data.Text as T

import qualified Web.Slack as Slack

import FOMObot.Helpers.Preferences
import FOMObot.Types.Bot
import FOMObot.Types.Command

processCommand :: Slack.SlackHandle -> Slack.Event -> Bot ()
processCommand handle (Slack.Message cid (Slack.UserComment uid) txt _ _ _) =
    case parseCommand $ T.unpack txt of
      (Add xs) -> addUserPrefs uid xs
      (Remove xs) -> removeUserPrefs uid xs
      List -> liftIO . Slack.sendMessage handle cid =<< (joinChannels <$> getUserPrefs uid)
      Stop -> deleteUserPrefs uid
      Help -> liftIO $ Slack.sendMessage handle cid helpText
      Unknown -> return ()
  where
    joinChannels :: [Slack.ChannelId] -> T.Text
    joinChannels [] = "No preferences set."
    joinChannels cids = "<#" <> T.intercalate "> <#" (view Slack.getId <$> cids) <> ">"

processCommand _ _ = return ()

helpText :: T.Text
helpText = "Possible Commands:\
    \\nadd [#channel ...] : Add channels that you would like to monitor for activity.\
    \\nremove [#channel ...] : Remove channels that you would no longer like to monitor.\
    \\nlist : List the channels you are monitoring for activity.\
    \\nstop : Stop FOMObot from monitoring any channels for activity.\
    \\nhelp : Print the help text."
