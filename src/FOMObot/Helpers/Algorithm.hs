module FOMObot.Helpers.Algorithm
    ( shiftInHistory
    , shiftInEvent
    , detectFOMOEvent
    ) where

import Control.Lens ((^.), (^?), (&), (.~), (%~), _head, view, views)
import qualified Data.List as List

import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState
import FOMObot.Types.HistoryItem

detectFOMOEvent :: ChannelState -> Bot Bool
detectFOMOEvent state = do
    density <- getDensity . configHistorySize <$> getConfig
    threshold <- configThreshold <$> getConfig

    case density of
        Nothing -> return False
        Just d -> return $ uniqueUsers >= 3 && d > threshold
  where
    uniqueUsers = views stateHistory (List.length . List.nub . List.map (view historyUserId)) state

    getDensity :: Int -> Maybe Density
    getDensity historySize = channelHistoryDensity historySize state

shiftInHistory :: BotConfig -> HistoryItem -> ChannelState -> ChannelState
shiftInHistory BotConfig{configHistorySize} historyItem s =
    if isFromPreviousUser
      then
        s & stateHistory . _head .~ historyItem
      else
        s & stateHistory %~ shiftIn configHistorySize historyItem
  where
    isFromPreviousUser = (s ^? stateHistory . _head . historyUserId) == Just (historyItem ^. historyUserId)

shiftInEvent :: BotConfig -> Bool -> ChannelState -> ChannelState
shiftInEvent BotConfig{configDebounceSize} event s =
    s & stateEventHistory %~ shiftIn configDebounceSize event

shiftIn :: Int -> a -> [a] -> [a]
shiftIn size item xs
    | isArrayFull xs size = item:List.init xs
    | otherwise = item:xs

isArrayFull :: [a] -> Int -> Bool
isArrayFull xs size = List.length xs == size
