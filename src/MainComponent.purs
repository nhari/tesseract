module MainComponent where

import Prelude

import CSS as CSS
import Card as C
import DOM.HTML.Indexed as DI
import Data.Array (intercalate, uncons)
import Data.Array as A
import Data.Function (on)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (head)
import Data.String as S
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

cardColorToStyles :: Array C.Color -> { bodyColor :: CSS.CSS, textBoxColor :: CSS.CSS }
cardColorToStyles [] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 198 208 209,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 223 228 224
}
cardColorToStyles [C.White] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 212 207 188,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 225 226 218
}
cardColorToStyles [C.Blue] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 108 156 194,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 166 193 202
}
cardColorToStyles [C.Black] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 42 45 43,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 172 160 146
}
cardColorToStyles [C.Red] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 207 113 85,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 217 180 154
}
cardColorToStyles [C.Green] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 118 140 104,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 177 182 159
}
cardColorToStyles _ = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 192 165 96,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 246 234 212
}

divWithClass :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
divWithClass className = HH.div [ HP.class_ $ H.ClassName className ]

iWithClass :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
iWithClass className = HH.i [ HP.class_ $ H.ClassName className ]

renderCard :: forall p r i.
  Array (HH.IProp DI.HTMLdiv i) -> C.Card -> HH.HTML p i
renderCard props = C.cardFaces >>> head >>> renderCardFace props

renderCardFace :: forall p r i.
  Array (HH.IProp DI.HTMLdiv i) -> C.CardFace -> HH.HTML p i
renderCardFace props (C.CardFace cf) =
  HH.div
    (props <> [ HC.style $ (cardColorToStyles cf.colors).bodyColor ])
    [
      divWithClass "card-body"
        [
          HH.div
            [
              HP.class_ $ H.ClassName "name-bar",
              HC.style $ (cardColorToStyles cf.colors).textBoxColor
            ]
            [
              divWithClass "card-name" [ HH.text cf.name ],
              divWithClass "spacer" [],
              divWithClass "card-cost" $ renderManaSymbol <$> cf.manaCost
            ],
          divWithClass "art"
            [HH.div
              [HP.class_ $ H.ClassName "art-image",
               HC.style $ CSS.backgroundImage $ CSS.url artUrl
              ]
              []
            ],
          HH.div
            [
              HP.class_ $ H.ClassName "type-bar",
              HC.style $ (cardColorToStyles cf.colors).textBoxColor
            ]
            [
              divWithClass "card-type" [ HH.text cardType ],
              divWithClass "spacer" [],
              divWithClass "edition-symbol"
                [ 
                  divWithClass "expansion" [ HH.text cf.setCode ],
                  divWithClass "rarity" [ HH.text rarity ]
                ]
            ],
          HH.div
            [
              HP.class_ $ H.ClassName "card-text",
              HC.style $ (cardColorToStyles cf.colors).textBoxColor
            ]
            [
              divWithClass "oracle-text" $ cf.text <#> \para ->
                divWithClass "oracle-paragraph" $ [ HH.text para ],
              divWithClass "spacer" [],
              divWithClass "flavor-text" [ HH.text cf.flavor ]
            ],
          pt
        ]
    ]
  where
    renderManaSymbol :: C.ManaSymbol -> HH.HTML p i
    renderManaSymbol (C.Generic n) = iWithClass ("ms ms-" <> show n <> " ms-cost") []
    renderManaSymbol C.GenericX = iWithClass ("ms ms-x ms-cost") []
    renderManaSymbol C.Colorless = iWithClass ("ms ms-c ms-cost") []
    renderManaSymbol C.Snow = iWithClass ("ms ms-s ms-cost") []
    renderManaSymbol (C.SingleColor c) = iWithClass ("ms ms-" <> c2s c <> " ms-cost") []
    renderManaSymbol (C.Phyrexian c) = iWithClass ("ms ms-p ms-cost") []
    renderManaSymbol (C.Hybrid c1 c2) = iWithClass ("ms ms-" <> c2s c1 <> c2s c2 <> " ms-split ms-cost") []
    renderManaSymbol (C.SingleColorHybrid c) = iWithClass ("ms ms-2" <> c2s c <> " ms-split ms-cost") []

    c2s :: C.Color -> String
    c2s C.White = "w"
    c2s C.Blue = "u"
    c2s C.Black = "b"
    c2s C.Red = "r"
    c2s C.Green = "g"

    types = intercalate " " cf.types
    cardType = case uncons cf.subtypes of
      Nothing -> types
      Just _  -> types <> " — " <> intercalate " " cf.subtypes
    artUrl = "https://magiccards.info/scans/en/"
      <> S.toLower cf.setCode
      <> "/" <> cf.number
      <> ".jpg"
    rarity = case cf.rarity of
      C.Common -> "(C)"
      C.Uncommon -> "(U)"
      C.Rare -> "(R)"
      C.MythicRare -> "(M)"
      C.BasicLand -> "(L)"
      C.Special -> "(S)"
    pt = case cf.characteristics of
      Just (C.PowerToughness { power, toughness }) ->
        HH.div
          [
            HP.class_ $ H.ClassName "pt",
            HC.style $ (cardColorToStyles cf.colors).textBoxColor
          ]
          [ HH.text $ power <> "/" <> toughness ]
      Just (C.Loyalty l) ->
        HH.div
          [
            HP.class_ $ H.ClassName "pt",
            HC.style $ (cardColorToStyles cf.colors).textBoxColor
          ]
          [ HH.text $ show l ]
      Nothing -> HH.div_ []

type State = {
  pack :: Array C.Card
, deck :: Array C.Card
}

data Query a
  = PickCard C.Card a
  | ReceiveCards (Array C.Card) a
  | ResetState a

data Message
  = PassedCards (Array C.Card)
  | EndOfPack

component :: forall m. H.Component HH.HTML Query Unit Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Unit -> State
  initialState = const { pack: [], deck: [] }

  render :: State -> H.ComponentHTML Query
  render state =
    divWithClass "draft-container"
      [
        divWithClass "pack-pane"
          [ HH.text "Pack",
            HH.button [ HE.onClick $ HE.input_ ResetState ] [ HH.text "Reset" ],
            HH.div_ $ map (\card ->
              renderCard [
                HP.class_ $ H.ClassName "card card-clickable",
                HE.onClick $ HE.input_ (PickCard card)
              ] card)
              state.pack
          ],
        divWithClass "deck-pane"
          [ HH.text "Deck",
            HH.div_ $ map (\card ->
              renderCard [ HP.class_ $ H.ClassName "card" ] card)
              state.deck
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (PickCard card next) = do
    st <- H.get
    H.modify (_ { pack = [], deck = st.deck `A.snoc` card })
    H.raise $ PassedCards $ A.filter (_ /= card) st.pack
    pure next
  eval (ReceiveCards cards next) = do
    H.modify (_ { pack = cards })
    when (A.null cards) $ H.raise EndOfPack
    pure next
  eval (ResetState next) = do
    H.put $ initialState unit
    H.raise EndOfPack
    pure next

