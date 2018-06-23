module FOMObot.Types.HistoryItem where

import Data.Text (Text)
import Control.Lens (makeLenses)
import qualified Web.Slack as Slack

data HistoryItem = HistoryItem
    { _historyTimeStamp :: Slack.SlackTimeStamp
    , _historyUserId :: Slack.UserId
    , _historyText :: Text
    } deriving (Show)

makeLenses ''HistoryItem
