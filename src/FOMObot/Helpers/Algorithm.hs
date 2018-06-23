module FOMObot.Helpers.Algorithm
    ( shiftInHistory
    , shiftInEvent
    , detectFOMOEvent
    ) where

import Control.Lens ((^.), (^?), (&), (.~), (%~), _head, view, views)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Text

import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState
import FOMObot.Types.HistoryItem

detectFOMOEvent :: BotConfig -> ChannelState -> Maybe Text
detectFOMOEvent config state =
    if uniqueUsers >= 3 && sufficientlyDense then
        Just $ channelHistoryText state
    else
        Nothing
  where
    uniqueUsers :: Int
    uniqueUsers = views stateHistory (List.length . List.nub . List.map (view historyUserId)) state

    sufficientlyDense :: Bool
    sufficientlyDense =
        configThreshold config < channelDensity

    channelDensity :: Density
    channelDensity =
        Maybe.fromMaybe 0 $ channelHistoryDensity (configHistorySize config) state


shiftInHistory :: BotConfig -> HistoryItem -> ChannelState -> ChannelState
shiftInHistory BotConfig{configHistorySize} historyItem s =
    if isFromPreviousUser then
      s & stateHistory . _head .~ historyItem
    else
      s & stateHistory %~ shiftIn configHistorySize historyItem
  where
    isFromPreviousUser = (s ^? stateHistory . _head . historyUserId) == Just (historyItem ^. historyUserId)

shiftInEvent :: BotConfig -> Maybe Text -> ChannelState -> ChannelState
shiftInEvent BotConfig{configDebounceSize} event s =
    s & stateEventHistory %~ shiftIn configDebounceSize event

shiftIn :: Int -> a -> [a] -> [a]
shiftIn size item xs
    | isArrayFull xs size = item:List.init xs
    | otherwise = item:xs

isArrayFull :: [a] -> Int -> Bool
isArrayFull xs size = List.length xs == size
