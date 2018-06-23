module FOMObot.Helpers.FOMOChannel
    ( isFOMOChannel
    , alertFOMOChannel
    , alertUsers
    ) where

import Control.Lens (uses, views, view, (^.), review)
import Data.List (find)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as T
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
    uids <- userIdsForChannel cid
    channels <- Maybe.catMaybes <$> mapM getDMChannel uids
    let channelIds = List.map getChannelId channels
    mapM_ alertUser channelIds
  where
    userIdsForChannel cid = Preferences.getUsersForChannel $ T.unpack $ cid ^. Slack.getId
    getChannelId = review Slack.getId . T.pack
    alertUser cid = Slack.sendMessage cid message
    message = "There's a party in <#" <> (cid ^. Slack.getId) <> ">!"
