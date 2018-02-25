module MtgJsonApi where

import Prelude hiding (between)

import Card as C
import Control.Apply (lift2)
import Control.Monad.Except (runExcept)
import Data.Array (filter, groupBy, many, nubBy, some, uncons)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors, isArray, readArray, readString)
import Data.Foreign.Class (class Decode, class Encode, encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromNumber, fromString)
import Data.List.NonEmpty as NE
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Natural (Natural, intToNat)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Set as S
import Data.StrMap (StrMap, values)
import Data.String (Pattern(..), fromCharArray, split)
import Data.Traversable (oneOf, traverse)
import Data.Validation.Semigroup (V, invalid, unV)
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (between, try)
import Text.Parsing.Parser.String (char, string)
import Text.Parsing.Parser.Token (digit)

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
derive instance newTypeSet :: Newtype Set _
derive instance genericSet :: Generic Set _
instance showSet :: Show Set where
  show = genericShow
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
instance showBoosterSlot :: Show BoosterSlot where
  show = genericShow
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
derive instance newTypeCard :: Newtype Card _
derive instance genericCard :: Generic Card _
instance showCard :: Show Card where
  show = genericShow
instance decodeCard :: Decode Card where
  decode = genericDecode options
instance encodeCard :: Encode Card where
  encode = genericEncode options

newtype Ruling = Ruling { date :: String, text :: String }
derive instance eqRuling :: Eq Ruling
derive instance ordRuling :: Ord Ruling
derive instance newTypeRuling :: Newtype Ruling _
derive instance genericRuling :: Generic Ruling _
instance showRuling :: Show Ruling where
  show = genericShow
instance decodeRuling :: Decode Ruling where
  decode = genericDecode options
instance encodeRuling :: Encode Ruling where
  encode = genericEncode options

newtype FormatLegality = FormatLegality { format :: String, legality :: String }
derive instance eqFormatLegality :: Eq FormatLegality
derive instance ordFormatLegality :: Ord FormatLegality
derive instance newTypeFormatLegality :: Newtype FormatLegality _
derive instance genericFormatLegality :: Generic FormatLegality _
instance showFormatLegality :: Show FormatLegality where
  show = genericShow
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
derive instance newTypeForeignPrinting :: Newtype ForeignPrinting _
derive instance genericForeignPrinting :: Generic ForeignPrinting _
instance showForeignPrinting :: Show ForeignPrinting where
  show = genericShow
instance decodeForeignPrinting :: Decode ForeignPrinting where
  decode = genericDecode options
instance encodeForeignPrinting :: Encode ForeignPrinting where
  encode = genericEncode options

parseCards :: String -> Either MultipleErrors (Array Card)
parseCards = runExcept <<< decodeJSON

parseSets :: String -> Either MultipleErrors AllSets
parseSets = runExcept <<< decodeJSON

type Errors = NonEmptyList String
error :: forall a. String -> V Errors a
error = NE.singleton >>> invalid

isDoubleFaced :: Card -> Boolean
isDoubleFaced (Card card) = card.layout == "double-faced"
  
isFlip :: Card -> Boolean
isFlip (Card card) = card.layout == "flip"

isSplit :: Card -> Boolean
isSplit (Card card) = card.layout == "split" || card.layout == "aftermath"

isSimple :: Card -> Boolean
isSimple card = not (isDoubleFaced card || isFlip card || isSplit card)

groupByNames :: Array Card -> Array (NonEmpty Array Card)
groupByNames = nubBy compareById >>> groupBy compareByNames
  where
    compareById = eq `on` (unwrap >>> (_.id))
    compareByNames = eq `on` (unwrap >>> (_.names))

parseAllSets :: AllSets -> Either Errors (Array C.Card)
parseAllSets allSets = unV Left Right $ map join $ traverse parseMtgJsonSet $ values allSets

parseMtgJsonSet :: Set -> V Errors (Array C.Card)
parseMtgJsonSet (Set set) =
  simpleCards `appendA` doubleFacedCards `appendA` flipCards `appendA` splitCards
  where
    appendA = lift2 append
    
    parseSimpleCard card = C.SimpleCard <$> parseCardFace set.code card
    simpleCards = traverse parseSimpleCard $ filter isSimple set.cards

    parseMultiPartCard makeCard (NonEmpty c1@(Card { layout: "meld" }) [c2, c3]) =
      makeCard c1 c3 `appendA` makeCard c2 c3
    parseMultiPartCard makeCard ne@(NonEmpty (Card { layout: "meld" }) _) =
      error $ "Meld cards must have exactly 3 entries: " <> show (map (unwrap >>> (_.names)) ne)
    parseMultiPartCard makeCard (NonEmpty c1 [c2]) = makeCard c1 c2
    parseMultiPartCard makeCard ne@(NonEmpty _ _) =
      error $ "Two-part cards must have exactly 2 entries: " <> show (map (unwrap >>> (_.names)) ne)
    
    parseDoubleFacedCard front back = A.singleton <$> C.DoubleFacedCard <$>
      ({ front: _, back: _ } <$> parseCardFace set.code front <*> parseCardFace set.code back)
    doubleFacedCards = map join <$>
      traverse (parseMultiPartCard parseDoubleFacedCard) $
      groupByNames $
      filter isDoubleFaced set.cards

    parseFlipCard top bottom = A.singleton <$> C.FlipCard <$>
      ({ top: _, bottom: _ } <$> parseCardFace set.code top <*> parseCardFace set.code bottom)
    flipCards = map join <$>
      traverse (parseMultiPartCard parseFlipCard) $
      groupByNames $
      filter isFlip set.cards
    
    parseSplitCard cards = C.SplitCard <$> traverse (parseCardFace set.code) cards
    splitCards = traverse parseSplitCard $ groupByNames $ filter isSplit set.cards

parseCardFace :: String -> Card -> V Errors C.CardFace
parseCardFace setCode (Card card) = C.CardFace <$> (makeRecord
    <$> parseManaCost (unwrap card.manaCost)
    <*> validateNatural "CMC" card.cmc
    <*> parseColors (unwrap card.colors)
    <*> parseTypes (unwrap card.types)
    <*> parseCharacteristics (unwrap card.power) (unwrap card.toughness) (unwrap card.loyalty)
    <*> parseRarity card.rarity
    <*> validateRequiredField "Number" (unwrap card.number)
  )
  where
    makeRecord = ({
        id: card.id,
        name: card.name,
        setCode,
        manaCost: _,
        cmc: _,
        colors: _,
        supertypes: fromMaybe [] $ unwrap card.supertypes,
        types: _,
        subtypes: fromMaybe [] $ unwrap card.subtypes,
        text: splitOracleText $ unwrap card.text,
        characteristics: _,
        flavor: fromMaybe "" $ unwrap card.flavor,
        rarity: _,
        number: _,
        artist: card.artist
    })

splitOracleText :: Maybe String -> Array String
splitOracleText Nothing   = []
splitOracleText (Just "") = []
splitOracleText (Just s)  = split (Pattern "\n") s

validateRequiredField :: forall a. String -> Maybe a -> V Errors a
validateRequiredField _ (Just a)        = pure a
validateRequiredField fieldName Nothing = error $ fieldName <> " must be present"

validateNatural :: String -> Number -> V Errors Natural
validateNatural fieldName num = case fromNumber num of
  Nothing -> error $ fieldName <> " must be an integer: " <> show num
  Just int ->
    if int < 0
    then error $ fieldName <> " must be non-negative: " <> show num
    else pure $ intToNat int

parseManaCost :: Maybe String -> V Errors (Array C.ManaSymbol)
parseManaCost Nothing = pure []
parseManaCost (Just s) =
  case runParser s $ many manaSymbol of
    Left err -> error $ "Invalid mana cost " <> show s <> ": " <> show err
    Right manaCost -> pure manaCost
  where
    colorChar :: Parser String C.Color
    colorChar = oneOf [
      char 'W' $> C.White,
      char 'U' $> C.Blue,
      char 'B' $> C.Black,
      char 'R' $> C.Red,
      char 'G' $> C.Green
    ]

    natural :: Parser String Natural
    natural = do
      digits <- some digit
      let numString = fromCharArray digits
      case fromString numString of
        Just int -> pure $ intToNat int
        -- shouldn't ever get here
        Nothing -> fail $ show numString <> " is not an int"

    manaSymbol :: Parser String C.ManaSymbol
    manaSymbol = between (char '{') (char '}') $ oneOf [
      try $ C.Hybrid <$> colorChar <*> (char '/' *> colorChar),
      try $ C.SingleColorHybrid <$> (string "2/" *> colorChar),
      try $ C.Phyrexian <$> (colorChar <* string "/P"),
      try $ C.Generic <$> natural,
      C.SingleColor <$> colorChar,
      char 'X' $> C.GenericX,
      char 'C' $> C.Colorless,
      char 'S' $> C.Snow
    ]

parseColors :: Maybe (Array String) -> V Errors (S.Set C.Color)
parseColors Nothing   = pure S.empty
parseColors (Just []) = pure S.empty
parseColors (Just cs) = S.fromFoldable <$> traverse parseColor cs
  where
    parseColor "White" = pure C.White
    parseColor "Blue"  = pure C.Blue
    parseColor "Black" = pure C.Black
    parseColor "Red"   = pure C.Red
    parseColor "Green" = pure C.Green
    parseColor s       = error $ "Invalid color " <> s

parseTypes :: Maybe (Array String) -> V Errors (NonEmpty Array String)
parseTypes Nothing   = error "Card has to have at least one type"
parseTypes (Just ts) = case uncons ts of
  Nothing -> error "Card has to have at least one type"
  Just { head, tail } -> pure $ NonEmpty head tail

parseCharacteristics :: Maybe String -> Maybe String -> Maybe Number -> V Errors (Maybe C.Characteristics)
parseCharacteristics (Just power) (Just toughness) Nothing = pure $ Just $ C.PowerToughness { power, toughness }
parseCharacteristics Nothing Nothing (Just loyalty) = Just <$> C.Loyalty <$> validateNatural "Loyalty" loyalty
parseCharacteristics (Just _) (Just _) (Just _) = error "Card can't have both power/toughness and loyalty"
parseCharacteristics (Just _) Nothing _ = error "Card can't have power without toughness"
parseCharacteristics Nothing (Just _) _ = error "Card can't have toughness without power"
parseCharacteristics _ _ _ = pure Nothing

parseRarity :: String -> V Errors C.Rarity
parseRarity "Common"      = pure C.Common
parseRarity "Uncommon"    = pure C.Uncommon
parseRarity "Rare"        = pure C.Rare
parseRarity "Mythic Rare" = pure C.MythicRare
parseRarity "Special"     = pure C.Special
parseRarity "Basic Land"  = pure C.BasicLand
parseRarity r             = error $ "Invalid rarity " <> r