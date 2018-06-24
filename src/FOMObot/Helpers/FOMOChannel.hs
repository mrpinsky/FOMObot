module FOMObot.Helpers.FOMOChannel
    ( isFOMOChannel
    , alertFOMOChannel
    , alertUsers
    ) where

import Control.Lens (uses, views, view)
import qualified Data.List as List (find)
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))
import Data.Text
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
    channelFinder = List.find (views Slack.channelName (== "fomo"))

alertFOMOChannel :: Slack.ChannelId -> Bot ()
alertFOMOChannel cid =
    view Slack.channelId <$> getFOMOChannel
    >>= alert message
  where
    message :: Text
    message = "<!here> There's a party in <#" <> view Slack.getId cid <> ">!"

alertUsers :: Slack.ChannelId -> Bot ()
alertUsers cid =
    Preferences.getUsersForChannel cid
    >>= getDMChannelIds
    >>= mapM_ (alert message)
  where
    getDMChannelIds :: [Slack.UserId] -> Bot [Slack.ChannelId]
    getDMChannelIds = fmap Maybe.catMaybes . mapM getDMChannel

    message :: Text
    message = "There's a party in <#" <> view Slack.getId cid <> ">!"

alert :: Text -> Slack.ChannelId -> Bot ()
alert message cid = Slack.sendMessage cid message
