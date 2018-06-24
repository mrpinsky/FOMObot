module FOMObot.Helpers.FOMOChannel
    ( isFOMOChannel
    , alertFOMOChannel
    , alertUsers
    ) where

import Control.Lens (uses, views, view, (^.))
import Data.List (find)
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))
import qualified Web.Slack as Slack
import qualified Web.Slack.Message as Slack

import FOMObot.Helpers.DMChannel (getDMChannel)
import qualified FOMObot.Helpers.Preferences as Preferences
import FOMObot.Types.Bot

isFOMOChannel :: Slack.ChannelId -> Bot Bool
isFOMOChannel cid = views Slack.channelId (== cid) <$> getFOMOChannel

getFOMOChannel :: Bot Slack.Channel
getFOMOChannel = do
    channels <- uses Slack.session $ view Slack.slackChannels
    return $ Maybe.fromJust $ channelFinder channels
  where
    channelFinder = find (views Slack.channelName (== "fomo"))

alertFOMOChannel :: Slack.ChannelId -> Bot ()
alertFOMOChannel channelID = do
    fomoChannel <- view Slack.channelId <$> getFOMOChannel
    Slack.sendMessage fomoChannel message
  where
    message = "<!here> There's a party in <#" <> (channelID ^. Slack.getId) <> ">!"

alertUsers :: Slack.ChannelId -> Bot ()
alertUsers cid = do
    uids <- Preferences.getUsersForChannel cid
    userDMChannels <- Maybe.catMaybes <$> mapM getDMChannel uids
    mapM_ alertUser userDMChannels
  where
    maybeChannelIds :: [Slack.UserId] -> Bot [Maybe Slack.ChannelId]
    maybeChannelIds = mapM getDMChannel

    alertUser channelId = Slack.sendMessage channelId message
    message = "There's a party in <#" <> view Slack.getId cid <> ">!"
