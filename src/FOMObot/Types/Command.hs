module FOMObot.Types.Command where

import Control.Lens (review)
import Data.Either (rights)
import qualified Data.Text as T
import qualified Data.List as List
import Text.Parsec (parse, manyTill, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, anyChar, char)
import qualified Web.Slack as Slack

data Command
    = Add [Slack.ChannelId]
    | Remove [Slack.ChannelId]
    | List
    | Stop
    | Help
    | Unknown
    deriving Show

parseCommand :: String -> Command
parseCommand s =
    case words s of
      "add":channels -> Add $ parseChannels channels
      "remove":channels -> Remove $ parseChannels channels
      "list":_ -> List
      "stop":_ -> Stop
      "help":_ -> Help
      _ -> Unknown
  where
    parseChannels :: [String] -> [Slack.ChannelId]
    parseChannels xs = List.map cidFromString $ rights $ parse parser "" <$> xs

    parser :: Parser String
    parser = string "<#" *> manyTill anyChar (char '>' <|> char '|')

    cidFromString :: String -> Slack.ChannelId
    cidFromString = review Slack.getId . T.pack
