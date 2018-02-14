module Card where

import Prelude (bind, not, pure, ($), (#), (<<<), (>>>), (<>), (>>=))
import Data.Array (filter, foldr)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Left, Right), note)
import Data.Tuple (snd)
import Data.Argonaut (
  class EncodeJson,
  class DecodeJson,
  decodeJson, encodeJson,
  fromString, toString)
import Data.Argonaut.Core (jsonEmptyObject, isNull, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Encode.Combinators (assoc, extend)
import Data.Argonaut.Decode.Combinators (getField, getFieldOptional, defaultField)

data Color = White | Blue | Black | Red | Green
instance encodeColor :: EncodeJson Color where
  encodeJson White = fromString "White"
  encodeJson Blue = fromString "Blue"
  encodeJson Black = fromString "Black"
  encodeJson Red = fromString "Red"
  encodeJson Green = fromString "Green"
instance decodeColor :: DecodeJson Color where
  decodeJson json = do
    jString <- note "Color must be a string" $ toString json
    case jString of
      "White" -> Right White
      "Blue" -> Right Blue
      "Black" -> Right Black
      "Red" -> Right Red
      "Green" -> Right Green
      other -> Left $ "Invalid color: " <> other

data Layout = Normal | DoubleFaced
instance encodeLayout :: EncodeJson Layout where
  encodeJson Normal = fromString "normal"
  encodeJson DoubleFaced = fromString "double-faced"
instance decodeLayout :: DecodeJson Layout where
  decodeJson json = do
    jString <- case toString json of
      Nothing -> Left "Layout must be a string"
      Just s -> Right s
    case jString of
      "normal" -> Right Normal
      "double-faced" -> Right DoubleFaced
      other -> Left $ "Invalid layout: " <> other

data Rarity = Common | Uncommon | Rare | MythicRare | BasicLand
instance encodeRarity :: EncodeJson Rarity where
  encodeJson Common = fromString "Common"
  encodeJson Uncommon = fromString "Uncommon"
  encodeJson Rare = fromString "Rare"
  encodeJson MythicRare = fromString "Mythic Rare"
  encodeJson BasicLand = fromString "Basic Land"
instance decodeRarity :: DecodeJson Rarity where
  decodeJson json = do
    jString <- case toString json of
      Nothing -> Left "Rarity must be a string"
      Just s -> Right s
    case jString of
      "Common" -> Right Common
      "Uncommon" -> Right Uncommon
      "Rare" -> Right Rare
      "Mythic Rare" -> Right MythicRare
      "Basic Land" -> Right BasicLand
      other -> Left $ "Invalid rarity: " <> other

newtype Card = Card {
  id :: String,
  name :: String,
  layout :: Layout,
  manaCost :: Maybe String,
  cmc :: Int,
  colors :: Array Color,
  types :: Array String,
  subtypes :: Array String,
  text :: Maybe String,
  power :: Maybe String,
  toughness :: Maybe String,
  loyalty :: Maybe Int,
  flavor :: Maybe String,
  rarity :: Rarity,
  number :: String,
  artist :: String
}
instance encodeCard :: EncodeJson Card where
  encodeJson (Card card) = foldr extend jsonEmptyObject nonNullFields
    where
      jsonFields = [
        assoc "id" card.id,
        assoc "name" card.name,
        assoc "layout" card.layout,
        assoc "manaCost" card.manaCost,
        assoc "cmc" card.cmc,
        assoc "colors" card.colors,
        assoc "types" card.types,
        assoc "subtypes" card.subtypes,
        assoc "text" card.text,
        assoc "power" card.power,
        assoc "toughness" card.toughness,
        assoc "loyalty" card.loyalty,
        assoc "rarity" card.rarity,
        assoc "flavor" card.flavor,
        assoc "number" card.number,
        assoc "artist" card.artist
      ]
      nonNullFields = jsonFields # filter (snd >>> isNull >>> not)
instance decodeCard :: DecodeJson Card where
  decodeJson json = do
    jsonObject <- decodeJson json -- decode into JObject
    id <- getField jsonObject "id"
    name <- getField jsonObject "name"
    layout <- getField jsonObject "layout"
    manaCost <- getFieldOptional jsonObject "manaCost"
    cmc <- getField jsonObject "cmc"
    colors <- getFieldOptional jsonObject "colors" `defaultField` []
    types <- getField jsonObject "types"
    subtypes <- getFieldOptional jsonObject "subtypes" `defaultField` []
    text <- getFieldOptional jsonObject "text"
    power <- getFieldOptional jsonObject "power"
    toughness <- getFieldOptional jsonObject "toughness"
    loyalty <- getFieldOptional jsonObject "loyalty"
    rarity <- getField jsonObject "rarity"
    flavor <- getFieldOptional jsonObject "flavor"
    number <- getField jsonObject "number"
    artist <- getField jsonObject "artist"
    pure $ Card {
      id,
      name,
      layout,
      manaCost,
      cmc,
      colors,
      types,
      subtypes,
      text,
      power,
      toughness,
      loyalty,
      rarity,
      flavor,
      number,
      artist
    }

cardToString :: Card -> String
cardToString = stringify <<< encodeJson

cardsToString :: Array Card -> String 
cardsToString = stringify <<< encodeJson

stringToCard :: String -> Either String Card
stringToCard string = jsonParser string >>= decodeJson

stringToCards :: String -> Either String (Array Card)
stringToCards string = jsonParser string >>= decodeJson