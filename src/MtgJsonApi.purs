module MtgJsonApi where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Foreign (MultipleErrors, isArray, readArray, readString)
import Data.Foreign.Class (class Decode, class Encode, encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)

options :: Options
options = defaultOptions { unwrapSingleConstructors = true }

type AllSets = StrMap Set

newtype Set = Set
  { name               :: String  -- ^ "Nemesis",       // The name of the set
  , code               :: String  -- ^ "NMS",           // The set's abbreviated code
  , gathererCode       :: NullOrUndefined String  -- ^ "NE",            // The code that Gatherer uses for the set. Only present if different than 'code'
  , oldCode            :: NullOrUndefined String  -- ^ "NEM",           // An old style code used by some Magic software. Only present if different than 'gathererCode' and 'code'
  , magicCardsInfoCode :: NullOrUndefined String  -- ^ "ne",            // The code that magiccards.info uses for the set. Only present if magiccards.info has this set
  , releaseDate        :: NullOrUndefined String  -- ^ "2000-02-14"     // When the set was released (YYYY-MM-DD). For promo sets, the date the first card was released.
  , border             :: String  -- ^ "black",         // The type of border on the cards, either "white", "black" or "silver"
  , type               :: String  -- ^ "expansion",     // Type of set. One of: "core", "expansion", "reprint", "box", "un", "from the vault", "premium deck", "duel deck", "starter", "commander", "planechase", "archenemy","promo", "vanguard", "masters", "conspiracy", "masterpiece"
  , block              :: NullOrUndefined String  -- ^ "Masques",       // The block this set is in,
  , onlineOnly         :: NullOrUndefined Boolean  -- ^ false,           // Present and set to true if the set was only released online
  , booster            :: NullOrUndefined (Array BoosterSlot)  -- ^ [ "rare", ... ], // Booster contents for this set, see below for details
  , cards              :: Array Card   -- ^ [ {}, {}, {}, ... ]  
}
derive instance eqSet :: Eq Set
derive instance ordSet :: Ord Set
derive instance genericSet :: Generic Set _
instance decodeSet :: Decode Set where
  decode = genericDecode options 
instance encodeSet :: Encode Set where
  encode = genericEncode options

data BoosterSlot 
  = BoosterSlotSingle   String 
  | BoosterSlotMultiple (Array String) 
derive instance eqBoosterSlot :: Eq BoosterSlot
derive instance ordBoosterSlot :: Ord BoosterSlot
derive instance genericBoosterSlot :: Generic BoosterSlot _
instance decodeBoosterSlot :: Decode BoosterSlot where
  decode foreignValue =
    if isArray foreignValue
    then do
      foreignArray <- readArray foreignValue
      BoosterSlotMultiple <$> traverse readString foreignArray
    else
      BoosterSlotSingle <$> readString foreignValue
instance encodeBoosterSlot :: Encode BoosterSlot where
  encode (BoosterSlotSingle a) = encode a
  encode (BoosterSlotMultiple as) = encode as

newtype Card = Card
  { id            :: String 
  , layout        :: String 
  , name          :: String 
  , names         :: NullOrUndefined (Array String) 
  , manaCost      :: NullOrUndefined String 
  , cmc           :: Number 
  , colors        :: NullOrUndefined (Array String) 
  , colorIdentity :: NullOrUndefined (Array String) 
  , type          :: String 
  , supertypes    :: NullOrUndefined (Array String) 
  , types         :: NullOrUndefined (Array String) -- ^ Un-cards can have no type 
  , subtypes      :: NullOrUndefined (Array String) 
  , rarity        :: String 
  , text          :: NullOrUndefined String 
  , flavor        :: NullOrUndefined String 
  , artist        :: String
  , number        :: NullOrUndefined String
  , power         :: NullOrUndefined String -- ^ Un-cards can have non-integer power/toughness 
  , toughness     :: NullOrUndefined String  
  , loyalty       :: NullOrUndefined Number 
  , multiverseid  :: NullOrUndefined Number
  , variations    :: NullOrUndefined (Array Number)
  , imageName     :: NullOrUndefined String 
  , watermark     :: NullOrUndefined String 
  , border        :: NullOrUndefined String 
  , timeshifted   :: NullOrUndefined Boolean -- IsCardTimeShifted
  , hand          :: NullOrUndefined Number  -- ^ Vanguard only 
  , life          :: NullOrUndefined Number -- ^ Vanguard only 
  , reserved      :: NullOrUndefined Boolean -- IsCardReserved 
  , releaseDate   :: NullOrUndefined String -- ^ Promo only 
  , starter       :: NullOrUndefined Boolean -- IsCardStarter 
  , mciNumber     :: NullOrUndefined String  -- ^ used by `MagicCards.info`, almost always identical to 'number' 
  , rulings       :: NullOrUndefined (Array Ruling) 
  , foreignNames  :: NullOrUndefined (Array ForeignPrinting) 
  , printings     :: Array String
  , originalText  :: NullOrUndefined String 
  , originalType  :: NullOrUndefined String
  , legalities    :: NullOrUndefined (Array FormatLegality)
  , source        :: NullOrUndefined String 
}
derive instance eqCard :: Eq Card
derive instance ordCard :: Ord Card
derive instance genericCard :: Generic Card _
instance decodeCard :: Decode Card where
  decode = genericDecode options
instance encodeCard :: Encode Card where
  encode = genericEncode options

newtype Ruling = Ruling { date :: String, text :: String }
derive instance eqRuling :: Eq Ruling
derive instance ordRuling :: Ord Ruling
derive instance genericRuling :: Generic Ruling _
instance decodeRuling :: Decode Ruling where
  decode = genericDecode options
instance encodeRuling :: Encode Ruling where
  encode = genericEncode options

newtype FormatLegality = FormatLegality { format :: String, legality :: String }
derive instance eqFormatLegality :: Eq FormatLegality
derive instance ordFormatLegality :: Ord FormatLegality
derive instance genericFormatLegality :: Generic FormatLegality _
instance decodeFormatLegality :: Decode FormatLegality where
  decode = genericDecode options
instance encodeFormatLegality :: Encode FormatLegality where
  encode = genericEncode options

newtype ForeignPrinting = ForeignPrinting {
  language :: String,
  name :: String,
  multiverseid :: NullOrUndefined Number
}
derive instance eqForeignPrinting :: Eq ForeignPrinting
derive instance ordForeignPrinting :: Ord ForeignPrinting
derive instance genericForeignPrinting :: Generic ForeignPrinting _
instance decodeForeignPrinting :: Decode ForeignPrinting where
  decode = genericDecode options
instance encodeForeignPrinting :: Encode ForeignPrinting where
  encode = genericEncode options

parseCards :: String -> Either MultipleErrors (Array Card)
parseCards = runExcept <<< decodeJSON

parseSets :: String -> Either MultipleErrors AllSets
parseSets = runExcept <<< decodeJSON

