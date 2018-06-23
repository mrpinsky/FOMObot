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

type Topic = Text

isFOMOChannel :: Slack.ChannelId -> Bot Bool
isFOMOChannel cid =
    maybe False (views Slack.channelId (== cid)) <$> getFOMOChannel

alertFOMOChannel :: Maybe Topic -> Slack.ChannelId -> Bot ()
alertFOMOChannel topic cid =
    getFOMOChannel
    >>= maybe (return ())  (alert message . view Slack.channelId)
  where
    message :: Text
    message = "<!here> " <> baseMessage topic cid

alertUsers :: Maybe Topic -> Slack.ChannelId -> Bot ()
alertUsers topic cid =
    Preferences.getUsersForChannel cid
    >>= getDMChannelIds
    >>= mapM_ (alert message)
  where
    getDMChannelIds :: [Slack.UserId] -> Bot [Slack.ChannelId]
    getDMChannelIds = fmap Maybe.catMaybes . mapM getDMChannel

    message :: Text
    message = baseMessage topic cid

alert :: Text -> Slack.ChannelId -> Bot ()
alert message cid = Slack.sendMessage cid message

baseMessage :: Maybe Text -> Slack.ChannelId -> Text
baseMessage maybeTopic cid =
    "There's a "
    <> maybe "" (<> " ") maybeTopic
    <> "party in <#"
    <> view Slack.getId cid
    <> "> and you're invited!"

getFOMOChannel :: Bot (Maybe Slack.Channel)
getFOMOChannel =
    channelFinder <$> slackChannels
  where
    slackChannels :: Bot [Slack.Channel]
    slackChannels = uses Slack.session (view Slack.slackChannels)

    channelFinder :: [Slack.Channel] -> Maybe Slack.Channel
    channelFinder = List.find (views Slack.channelName (== "fomo"))
