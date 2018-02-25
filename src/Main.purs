module Main where

import Prelude

import Card as C
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS
import Data.Array (length, replicate, (!!))
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.Foreign (F, Foreign, toForeign, readString)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainComponent as MC
import MtgJsonApi as API
import Network.HTTP.Affjax (AJAX, get)
import Partial.Unsafe (unsafePartial)

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
-- shuffle is too slow with this many cards
generatePack cards = sequence $ replicate 15 do
  index <- randomInt 0 $ length cards
  unsafePartial $ case cards !! index of
    Just card -> pure card

type MainEffects = HA.HalogenEffects (
  dom :: DOM,
  ajax :: AJAX,
  console :: CONSOLE,
  random :: RANDOM
)

main :: Eff MainEffects Unit
main = launchAff_ do
  body <- HA.awaitBody
  io <- runUI MC.component unit body

  res <- get "./sets.json"
  liftEff case API.readCardsFromJson res.response of
    Left errors -> do
      log $ "Error parsing data: "
      traverse_ log errors
      pure unit
    Right cards -> do
      connection <- WS.create (WS.URL "wss://echo.websocket.org") []
      launchAff_ do
        -- Send the inital cards
        pack <- liftEff $ generatePack cards
        io.query $ MC.ReceiveCards pack unit

        -- Handle messages from the component
        io.subscribe $ CR.consumer \msg -> do
          case msg of
            MC.PassedCards msgContents ->
              liftEff $ WS.sendString connection $ C.writeCards msgContents
            MC.EndOfPack -> do
              pack <- liftEff $ generatePack cards
              io.query $ MC.ReceiveCards pack unit
          pure Nothing

        -- Listen to messages from the websocket
        CR.runProcess $ CR.connect (wsProducer connection) $ CR.consumer \msg ->
          case C.readCards msg of
            Right cards -> do
              io.query $ H.action $ MC.ReceiveCards cards
              pure Nothing
            Left error -> do
              liftEff $ throw $ "Error parsing message from websocket: " <> error <> " (" <> msg <> ")"