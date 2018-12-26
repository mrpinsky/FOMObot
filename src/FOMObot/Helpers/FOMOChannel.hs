module FOMObot.Helpers.FOMOChannel
    ( isFOMOChannel
    , alertFOMOChannel
    , alertUsers
    ) where

import Control.Lens (views, view)
import Control.Monad.IO.Class (liftIO)

import qualified Data.List as List (find)
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))
import Data.Text

import qualified Web.Slack as Slack

import FOMObot.Helpers.DMChannel (getDMChannel)
import qualified FOMObot.Helpers.Preferences as Preferences
import FOMObot.Types.Bot

type Topic = Text

isFOMOChannel :: Slack.SlackHandle -> Slack.ChannelId -> Bool
isFOMOChannel handle cid =
    maybe False checkId $ getFOMOChannel handle
  where
    checkId :: Slack.Channel -> Bool
    checkId = views Slack.channelId (== cid)

alertFOMOChannel :: Slack.SlackHandle -> Maybe Topic -> Slack.ChannelId -> Bot ()
alertFOMOChannel handle topic cid =
    maybe (return ()) sendAlert $ getFOMOChannel handle
  where
    sendAlert :: Slack.Channel -> Bot ()
    sendAlert = alert handle message . view Slack.channelId

    message :: Text
    message = "<!here> " <> baseMessage topic cid

alertUsers :: Slack.SlackHandle -> Maybe Topic -> Slack.ChannelId -> Bot ()
alertUsers handle topic cid =
    Preferences.getUsersForChannel cid
    >>= getDMChannelIds
    >>= mapM_ (alert handle message)
  where
    getDMChannelIds :: [Slack.UserId] -> Bot [Slack.ChannelId]
    getDMChannelIds = fmap Maybe.catMaybes . mapM getDMChannel

    message :: Text
    message = baseMessage topic cid

alert :: Slack.SlackHandle -> Text -> Slack.ChannelId -> Bot ()
alert handle message cid =
    liftIO $ Slack.sendMessage handle cid message

baseMessage :: Maybe Text -> Slack.ChannelId -> Text
baseMessage maybeTopic cid =
    "There's a "
    <> maybe "" (<> " ") maybeTopic
    <> "party in <#"
    <> view Slack.getId cid
    <> "> and you're invited!"

getFOMOChannel :: Slack.SlackHandle -> Maybe Slack.Channel
getFOMOChannel handle =
    channelFinder slackChannels
  where
    slackChannels :: [Slack.Channel]
    slackChannels = view Slack.slackChannels $ Slack.getSession handle

    channelFinder :: [Slack.Channel] -> Maybe Slack.Channel
    channelFinder = List.find (views Slack.channelName (== "fomo"))
