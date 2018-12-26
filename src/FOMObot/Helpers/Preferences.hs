module FOMObot.Helpers.Preferences
    ( addUserPrefs
    , getUserPrefs
    , deleteUserPrefs
    , removeUserPrefs
    , getUsersForChannel
    ) where

import Control.Lens (view, review)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Database.Redis as R
import qualified Web.Slack as Slack

import FOMObot.Types.Bot

addUserPrefs :: Slack.UserId -> [Slack.ChannelId] -> Bot ()
addUserPrefs uid prefs = do
    -- add channel prefs to user
    void $ R.liftRedis $ R.sadd (userPrefsKey uid) $ channelNameByteString <$> prefs
    -- add user to each channel
    mapM_ (addUserToChannel uid) prefs

getUserPrefs :: Slack.UserId -> Bot [Slack.ChannelId]
getUserPrefs uid =
    either (const []) (map idFromByteString)
    <$> fetchFromRedis uid
  where
    fetchFromRedis :: Slack.UserId -> Bot (Either R.Reply [ByteString])
    fetchFromRedis = R.liftRedis . R.smembers . userPrefsKey

deleteUserPrefs :: Slack.UserId -> Bot ()
deleteUserPrefs uid = do
    -- remove user from each channel
    mapM_ (removeUserFromChannel uid) =<< getUserPrefs uid
    -- delete user prefs
    void $ R.liftRedis $ R.del [userPrefsKey uid]

removeUserPrefs :: Slack.UserId -> [Slack.ChannelId] -> Bot ()
removeUserPrefs uid prefs = do
    -- remove channel prefs from user
    void $ R.liftRedis $ R.srem (userPrefsKey uid) $ channelNameByteString <$> prefs
    -- remove user from each channel
    mapM_ (removeUserFromChannel uid) prefs

getUsersForChannel :: Slack.ChannelId -> Bot [Slack.UserId]
getUsersForChannel cid =
    either (const []) (map idFromByteString)
    <$> R.liftRedis (R.smembers $ channelKey cid)

addUserToChannel :: Slack.UserId -> Slack.ChannelId -> Bot ()
addUserToChannel uid = void . R.liftRedis . (`R.sadd` [userIdByteString uid]) . channelKey

removeUserFromChannel :: Slack.UserId -> Slack.ChannelId -> Bot ()
removeUserFromChannel uid = void . R.liftRedis . (`R.srem` [userIdByteString uid]) . channelKey

userPrefsKey :: Slack.UserId -> ByteString
userPrefsKey uid = userKey uid <> ":prefs"

userIdByteString :: Slack.UserId -> ByteString
userIdByteString = encodeUtf8 . view Slack.getId

userKey :: Slack.UserId -> ByteString
userKey uid = "users:" <> userIdByteString uid

channelKey :: Slack.ChannelId -> ByteString
channelKey cid =
    "channels:" <> channelNameByteString cid <> ":users"

channelNameByteString :: Slack.ChannelId -> ByteString
channelNameByteString = pack . T.unpack . view Slack.getId

idFromByteString :: ByteString -> Slack.Id a
idFromByteString = review Slack.getId . decodeUtf8
