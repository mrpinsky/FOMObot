module FOMObot.Types.ChannelState where

import Control.Lens (makeLenses, view, _head, _last, (^.), (^?), (&), (.~), (%~))

import qualified Data.Text as T
import qualified Data.List as List

import qualified Web.Slack as Slack

import FOMObot.Types.BotConfig
import FOMObot.Types.HistoryItem

data ChannelState = ChannelState
    { _stateHistory :: [HistoryItem]
    , _stateEventHistory :: [Maybe T.Text]
    } deriving (Show)

makeLenses ''ChannelState

channelHistoryText :: ChannelState -> T.Text
channelHistoryText = view (stateHistory . traverse . historyText)

type Density = Double

channelHistoryDensity :: BotConfig -> ChannelState -> Maybe Density
channelHistoryDensity config state =
    stateHistoryWhenFull maxHistorySize
        >>= historyDuration
        >>= Just . eventsPerMinute (fromIntegral maxHistorySize)
  where
    maxHistorySize :: Int
    maxHistorySize = configHistorySize config

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


shiftInHistory :: BotConfig -> HistoryItem -> ChannelState -> ChannelState
shiftInHistory BotConfig{configHistorySize} historyItem s =
    if isFromPreviousUser then
      s & stateHistory . _head .~ historyItem
    else
      s & stateHistory %~ shiftIn configHistorySize historyItem
  where
    isFromPreviousUser = (s ^? stateHistory . _head . historyUserId) == Just (historyItem ^. historyUserId)

shiftInEvent :: BotConfig -> Maybe T.Text -> ChannelState -> ChannelState
shiftInEvent BotConfig{configDebounceSize} event state =
    state & stateEventHistory %~ shiftIn configDebounceSize event

shiftIn :: Int -> a -> [a] -> [a]
shiftIn size item xs
    | isArrayFull xs size = item:List.init xs
    | otherwise = item:xs

isArrayFull :: [a] -> Int -> Bool
isArrayFull xs size = List.length xs == size
