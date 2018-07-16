module FOMObot.Helpers.MessageProcessor
    ( processMessage
    ) where

import Control.Lens (views, (^.))
import qualified Data.Text as T
import qualified Web.Slack as Slack

import FOMObot.Helpers.Algorithm
import FOMObot.Types.Bot
import FOMObot.Types.ChannelState
import FOMObot.Types.HistoryItem

processMessage :: Slack.Event -> Bot Bool
processMessage (Slack.Message channelID (Slack.UserComment userID) _ messageTimestamp _ _) = do
    config <- getConfig

    -- Add the message timestamp to the channel state
    channelState <- shiftInHistory config historyItem
        <$> botChannelState messageChannelID

    -- Detect an event that surpasses the threshold
    eventOccurred <- detectFOMOEvent channelState

    -- Save the channel state after adding the event status
    botSaveState messageChannelID
        $ shiftInEvent config eventOccurred channelState

    -- Signal an event only if an event occured and no recent events
    let recentlyNotified = views stateEventHistory or channelState
    return $ eventOccurred && not recentlyNotified
  where
    messageChannelID :: String
    messageChannelID = T.unpack $ channelID ^. Slack.getId

    historyItem :: HistoryItem
    historyItem = HistoryItem messageTimestamp userID

processMessage _ = return False
