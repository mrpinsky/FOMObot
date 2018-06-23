module FOMObot.Types.ChannelState where

import qualified Data.Text as T
import Control.Lens (makeLenses, view, _head, _last, (^.), (^?))
import qualified Web.Slack as Slack

import FOMObot.Types.HistoryItem

data ChannelState = ChannelState
    { _stateHistory :: [HistoryItem]
    , _stateEventHistory :: [Maybe T.Text]
    } deriving (Show)

makeLenses ''ChannelState

channelHistoryText :: ChannelState -> T.Text
channelHistoryText = view (stateHistory . traverse . historyText)

type Density = Double

channelHistoryDensity :: Int -> ChannelState -> Maybe Density
channelHistoryDensity historySize state =
    stateHistoryWhenFull historySize
        >>= historyDuration
        >>= Just . eventsPerMinute (fromIntegral historySize)
  where
    stateHistoryWhenFull :: Int -> Maybe [HistoryItem]
    stateHistoryWhenFull maxSize = if length channelHistory == maxSize then
        Just channelHistory
    else
        Nothing

    channelHistory :: [HistoryItem]
    channelHistory = state ^. stateHistory

    eventsPerMinute :: Int -> Slack.Time -> Density
    eventsPerMinute eventsCount durationInSeconds = 60 * fromIntegral eventsCount / realToFrac durationInSeconds

    historyDuration :: [HistoryItem] -> Maybe Slack.Time
    historyDuration history = do
        lts <- history ^? _head . historyTimeStamp . Slack.slackTime
        ets <- history ^? _last . historyTimeStamp . Slack.slackTime
        return $ lts - ets
