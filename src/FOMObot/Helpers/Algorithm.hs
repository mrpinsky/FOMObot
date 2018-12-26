module FOMObot.Helpers.Algorithm where

import Control.Lens (view, views)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text

import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState
import FOMObot.Types.HistoryItem

detectFOMOEvent :: BotConfig -> ChannelState -> Maybe Text
detectFOMOEvent config state =
    if enoughUsers && denseEnough then
        Just $ channelHistoryText state
    else
        Nothing
    where
        enoughUsers :: Bool
        enoughUsers = uniqueUsers state >= minUniqueUsers

        denseEnough :: Bool
        denseEnough = sufficientlyDense config state

        minUniqueUsers :: Int
        minUniqueUsers = configMinUniqueUsers config


uniqueUsers :: ChannelState -> Int
uniqueUsers =
    views stateHistory (List.length . List.nub . List.map (view historyUserId))


sufficientlyDense :: BotConfig -> ChannelState -> Bool
sufficientlyDense config state =
    configThreshold config < channelDensity config state


channelDensity :: BotConfig -> ChannelState -> Density
channelDensity config state =
    Maybe.fromMaybe 0 $ channelHistoryDensity config state
