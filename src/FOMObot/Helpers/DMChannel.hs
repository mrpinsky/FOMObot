module FOMObot.Helpers.DMChannel
    ( setDMChannel
    , isDMChannel
    , getDMChannel
    ) where

import Control.Lens (review, view, views)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Database.Redis as R
import qualified Web.Slack as Slack

import FOMObot.Types.Bot

setDMChannel :: Slack.UserId -> Slack.IMId -> Bot ()
setDMChannel uid cid = void $ R.liftRedis $ R.set (dmChannelKey uid) dmChannelValue
  where
    dmChannelValue = encodeUtf8 $ view Slack.getId cid

isDMChannel :: Slack.UserId -> Slack.ChannelId -> Bot Bool
isDMChannel uid cid = do
    channelId <- R.liftRedis $ R.get (dmChannelKey uid)
    return $ channelId == views Slack.getId (Right . Just . encodeUtf8) cid

getDMChannel :: Slack.UserId -> Bot (Maybe Slack.ChannelId)
getDMChannel uid = either (const Nothing) (fmap cidFromByteString)
    <$> fetchFromRedis uid
  where
    fetchFromRedis :: Slack.UserId -> Bot (Either R.Reply (Maybe ByteString))
    fetchFromRedis userId = R.liftRedis $ R.get $ dmChannelKey userId

    cidFromByteString = review Slack.getId . T.pack . unpack

dmChannelKey :: Slack.UserId -> ByteString
dmChannelKey uid = userKey uid <> ":channel"

userKey :: Slack.UserId -> ByteString
userKey uid = "users:" <> encodeUtf8 (view Slack.getId uid)
