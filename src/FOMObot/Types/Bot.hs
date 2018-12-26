{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FOMObot.Types.Bot where

import qualified System.IO as IO

import Control.Lens (set)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State

import qualified Data.HashMap as HM
import Data.Text

import Database.Redis (MonadRedis(..), runRedis, connect)

import FOMObot.Types.AppState
import FOMObot.Types.BotConfig
import FOMObot.Types.BotState
import FOMObot.Types.ChannelState

newtype Bot a = Bot {runBot :: StateT AppState IO a}
  deriving (Monad, Functor, Applicative, MonadState AppState, MonadIO)

instance MonadRedis Bot where
    liftRedis f = do
        BotConfig{configRedisConnection} <- getConfig
        connection <- liftIO $ connect configRedisConnection
        liftIO $ runRedis connection f

initialState :: Bot AppState
initialState = liftIO $ FOMObot.Types.AppState.init <$> buildConfig

getConfig :: Bot BotConfig
getConfig = fmap _botConfig get

getState :: Bot BotState
getState = fmap _botState get

modifyState :: (BotState -> BotState) -> Bot ()
modifyState f = modify . set botState . f =<< getState

botChannelState :: String -> Bot ChannelState
botChannelState channelID =
    HM.lookup channelID <$> getState
    >>= maybe (botInsert channelID) return

botInsert :: String -> Bot ChannelState
botInsert channelID = do
    let newChannelState = ChannelState [] []
    botSaveState channelID newChannelState
    return newChannelState

botSaveState :: String -> ChannelState -> Bot ()
botSaveState channelID = modifyState . HM.insert channelID

botLog :: Text -> Bot ()
botLog msg =
    liftIO $ IO.putStrLn $ unpack msg
