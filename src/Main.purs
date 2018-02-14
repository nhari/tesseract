module Main where

import Prelude

import Card as C
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS
import Data.Array (take)
import Data.Array.Shuffle (shuffle)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, toForeign, readString)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainComponent as MC
import Network.HTTP.Affjax (AJAX, get)

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer
  :: forall eff
   . WS.WebSocket
  -> CR.Producer String (Aff (avar :: AVAR, exception :: EXCEPTION, dom :: DOM | eff)) Unit
wsProducer socket = CRA.produce \emit ->
  EET.addEventListener
    WSET.onMessage
    (listener emit)
    false
    (WS.socketToEventTarget socket)
  where
    listener emit = EET.eventListener \ev -> do
      for_ (readHelper WS.readMessageEvent ev) \msgEvent ->
        for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
          emit (Left msg)
    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< toForeign

generatePack :: forall eff. Array C.Card -> Eff (random :: RANDOM | eff) (Array C.Card)
generatePack cards = take 15 <$> shuffle cards

type MainEffects = HA.HalogenEffects (
  dom :: DOM,
  ajax :: AJAX,
  console :: CONSOLE,
  random :: RANDOM
)
main :: Eff MainEffects Unit
main = launchAff_ do
  res <- get "./cards.json"
  liftEff $ case C.stringToCards res.response of
    Left error -> do
      log $ "Error parsing data: " <> error
      pure unit
    Right cards -> do
      connection <- WS.create (WS.URL "wss://echo.websocket.org") []
      HA.runHalogenAff do
        body <- HA.awaitBody
        io <- runUI MC.component unit body

        -- Send the inital cards
        pack <- liftEff $ generatePack cards
        io.query $ MC.ReceiveCards pack unit

        -- Handle messages from the component
        io.subscribe $ CR.consumer \msg -> do
          case msg of
            MC.PassedCards msgContents ->
              liftEff $ WS.sendString connection $ C.cardsToString msgContents
            MC.EndOfPack -> do
              pack <- liftEff $ generatePack cards
              io.query $ MC.ReceiveCards pack unit
          pure Nothing

        -- Listen to messages from the websocket
        CR.runProcess $ CR.connect (wsProducer connection) $ CR.consumer \msg ->
          case C.stringToCards msg of
            Right cards -> do
              io.query $ H.action $ MC.ReceiveCards cards
              pure Nothing
            Left error -> do
              liftEff $ throw $ "Error parsing message from websocket: " <> error <> " (" <> msg <> ")"