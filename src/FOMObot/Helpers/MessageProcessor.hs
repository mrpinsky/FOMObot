module FOMObot.Helpers.MessageProcessor
    ( processMessage
    ) where

import Control.Lens (views, (^.))

import qualified Data.Text as T
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))

import qualified Web.Slack as Slack

import FOMObot.Helpers.Algorithm
import FOMObot.Types.Bot
import FOMObot.Types.ChannelState
import FOMObot.Types.HistoryItem

processMessage :: Slack.Event -> Bot (Maybe T.Text)
processMessage (Slack.Message channelID (Slack.UserComment userID) messageText messageTimestamp _ _) = do
    botLog $ T.intercalate " "
        [ "Channel"
        , Slack._getId channelID
        , "received message from user"
        , Slack._getId userID
        , ":\\n"
        , messageText
        ]

    config <- getConfig

    -- Add the message timestamp to the channel state
    channelState <- shiftInHistory config historyItem
        <$> botChannelState messageChannelID

    botLog $ T.intercalate " "
        [ "Channel"
        , Slack._getId channelID
        , "has state\\n"
        , T.pack $ show channelState
        ]

    -- Detect an event that surpasses the threshold
    let maybeEventText = detectFOMOEvent config channelState

    botLog $ T.intercalate " "
        [ "Channel"
        , Slack._getId channelID
        , loggableEventText maybeEventText
        ]

    -- Save the channel state after adding the event status
    botSaveState messageChannelID
        $ shiftInEvent config maybeEventText channelState

    -- Signal an event only if an event occured and no recent events
    let recentEvents = views stateEventHistory Maybe.catMaybes channelState

    botLog $ T.intercalate " "
        [ "Channel"
        , Slack._getId channelID
        , if null recentEvents then "did not" else "did"
        , "experience an event recently"
        ]

    return $ if null recentEvents then maybeEventText else Nothing
  where
    messageChannelID :: String
    messageChannelID = T.unpack $ channelID ^. Slack.getId

    historyItem :: HistoryItem
    historyItem = HistoryItem messageTimestamp userID $ T.pack $ show messageText

    loggableEventText :: Maybe T.Text -> T.Text
    loggableEventText Nothing = "did not experience an event"
    loggableEventText (Just text) =
        "experienced an event with text:\\n" <> text

processMessage _ = return Nothing
