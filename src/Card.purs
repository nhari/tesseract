module Card where

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.NonEmpty (NonEmpty(..))
import Data.Set as S
import Data.Natural (Natural)
import Prelude (class Eq, class Ord, class Show)

data Color = White | Blue | Black | Red | Green
derive instance genericColor :: Generic Color _
derive instance eqColor :: Eq Color
derive instance ordColor :: Ord Color
instance showColor :: Show Color where
  show = genericShow
instance encodeColor :: Encode Color where
  encode = genericEncode defaultOptions
instance decodeColor :: Decode Color where
  decode = genericDecode defaultOptions

data Rarity = BasicLand | Common | Uncommon | Rare | MythicRare | Special
derive instance genericRarity :: Generic Rarity _
derive instance eqRarity :: Eq Rarity
derive instance ordRarity :: Ord Rarity
instance showRarity :: Show Rarity where
  show = genericShow
instance encodeRarity :: Encode Rarity where
  encode = genericEncode defaultOptions
instance decodeRarity :: Decode Rarity where
  decode = genericDecode defaultOptions

data Card = 
  SimpleCard CardFace |
  DoubleFacedCard { front :: CardFace, back :: CardFace } |
  FlipCard { top :: CardFace, bottom :: CardFace } |
  SplitCard (NonEmpty Array CardFace)
derive instance genericCard :: Generic Card _
derive instance eqCard :: Eq Card
derive instance ordCard :: Ord Card
instance showCard :: Show Card where
  show = genericShow
-- instance encodeCard :: Encode Card where
--   encode = genericEncode defaultOptions
-- instance decodeCard :: Decode Card where
--   decode = genericDecode defaultOptions

data ManaSymbol =
  Generic Natural |       -- {1}
  GenericX |              -- {X}
  Colorless |             -- {C}
  Snow |                  -- {S}
  SingleColor Color |     -- {W}
  Phyrexian Color |       -- {W/P}
  Hybrid Color Color |    -- {W/U}
  SingleColorHybrid Color -- {2/W}
derive instance genericManaSymbol :: Generic ManaSymbol _
derive instance eqManaSymbol :: Eq ManaSymbol
derive instance ordManaSymbol :: Ord ManaSymbol
instance showManaSymbol :: Show ManaSymbol where
  show ms = genericShow ms
-- instance encodeManaSymbol :: Encode ManaSymbol where
--   encode ms = genericEncode defaultOptions ms
-- instance decodeManaSymbol :: Decode ManaSymbol where
--   decode ms = genericDecode defaultOptions ms

data Characteristics =
  PowerToughness { power :: String, toughness :: String } |
  Loyalty Natural
derive instance genericCharacteristics :: Generic Characteristics _
derive instance eqCharacteristics :: Eq Characteristics
derive instance ordCharacteristics :: Ord Characteristics
instance showCharacteristics :: Show Characteristics where
  show = genericShow
-- instance encodeCharacteristics :: Encode Characteristics where
--   encode = genericEncode defaultOptions
-- instance decodeCharacteristics :: Decode Characteristics where
--   decode = genericDecode defaultOptions

type OracleParagraph = String
newtype CardFace = CardFace {
  id :: String,
  name :: String,
  setCode :: String,
  manaCost :: Array ManaSymbol,
  cmc :: Natural,
  colors :: S.Set Color,
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
derive instance genericCardFace :: Generic CardFace _
derive instance eqCardFace :: Eq CardFace
derive instance ordCardFace :: Ord CardFace
instance showCardFace :: Show CardFace where
  show = genericShow
-- instance encodeCardFace :: Encode CardFace where
--   encode = genericEncode defaultOptions
-- instance decodeCardFace :: Decode CardFace where
--   decode = genericDecode defaultOptions

cardFaces :: Card -> NonEmpty Array CardFace
cardFaces (SimpleCard cf) = NonEmpty cf []
cardFaces (DoubleFacedCard { front, back }) = NonEmpty front [back]
cardFaces (FlipCard { top, bottom }) = NonEmpty top [bottom]
cardFaces (SplitCard cfs) = cfs