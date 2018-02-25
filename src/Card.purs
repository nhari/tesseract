module Card where

import Prelude

import Data.Argonaut (jsonParser)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Natural (Natural)
import Data.NonEmpty (NonEmpty(..))

data Color = White | Blue | Black | Red | Green
derive instance genericColor :: Generic Color
derive instance eqColor :: Eq Color
derive instance ordColor :: Ord Color
instance showColor :: Show Color where
  show = gShow

data Rarity = BasicLand | Common | Uncommon | Rare | MythicRare | Special
derive instance genericRarity :: Generic Rarity
derive instance eqRarity :: Eq Rarity
derive instance ordRarity :: Ord Rarity
instance showRarity :: Show Rarity where
  show = gShow

data Card = 
  SimpleCard CardFace |
  DoubleFacedCard { front :: CardFace, back :: CardFace } |
  FlipCard { top :: CardFace, bottom :: CardFace } |
  SplitCard (NonEmpty Array CardFace)
derive instance genericCard :: Generic Card
derive instance eqCard :: Eq Card
derive instance ordCard :: Ord Card
instance showCard :: Show Card where
  show = gShow

data ManaSymbol =
  Generic Natural |       -- {1}
  GenericX |              -- {X}
  Colorless |             -- {C}
  Snow |                  -- {S}
  SingleColor Color |     -- {W}
  Phyrexian Color |       -- {W/P}
  Hybrid Color Color |    -- {W/U}
  SingleColorHybrid Color -- {2/W}
derive instance genericManaSymbol :: Generic ManaSymbol
derive instance eqManaSymbol :: Eq ManaSymbol
derive instance ordManaSymbol :: Ord ManaSymbol
instance showManaSymbol :: Show ManaSymbol where
  show ms = gShow ms

data Characteristics =
  PowerToughness { power :: String, toughness :: String } |
  Loyalty Natural
derive instance genericCharacteristics :: Generic Characteristics
derive instance eqCharacteristics :: Eq Characteristics
derive instance ordCharacteristics :: Ord Characteristics
instance showCharacteristics :: Show Characteristics where
  show = gShow

type OracleParagraph = String
newtype CardFace = CardFace {
  id :: String,
  name :: String,
  setCode :: String,
  manaCost :: Array ManaSymbol,
  cmc :: Natural,
  colors :: Array Color, -- Should be a Set but no Generic instance...
  supertypes :: Array String,
  types :: NonEmpty Array String,
  subtypes :: Array String,
  text :: Array OracleParagraph,
  characteristics :: Maybe Characteristics,
  flavor :: String,
  rarity :: Rarity,
  number :: String,
  artist :: String
}
derive instance genericCardFace :: Generic CardFace
derive instance eqCardFace :: Eq CardFace
derive instance ordCardFace :: Ord CardFace
instance showCardFace :: Show CardFace where
  show = gShow

cardFaces :: Card -> NonEmpty Array CardFace
cardFaces (SimpleCard cf) = NonEmpty cf []
cardFaces (DoubleFacedCard { front, back }) = NonEmpty front [back]
cardFaces (FlipCard { top, bottom }) = NonEmpty top [bottom]
cardFaces (SplitCard cfs) = cfs

writeCard :: Card -> String
writeCard = encodeJson >>> show

writeCards :: Array Card -> String
writeCards = encodeJson >>> show

readCard :: String -> Either String Card
readCard s = jsonParser s >>= decodeJson

readCards :: String -> Either String (Array Card)
readCards s = jsonParser s >>= decodeJson