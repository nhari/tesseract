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
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainComponent as MC
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX)

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

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ReceiveCards` queries in when it receives inputs from the
-- producer.
wsConsumer
  :: forall eff
   . (MC.Query ~> Aff (HA.HalogenEffects eff))
  -> CR.Consumer String (Aff (HA.HalogenEffects eff)) Unit
wsConsumer query = CR.consumer \msg -> case C.stringToCards msg of
    Right cards -> do
      query $ H.action $ MC.ReceiveCards cards
      pure Nothing
    Left error -> do
      liftEff $ throw $ "Error parsing message from websocket: " <> error <> " (" <> msg <> ")"

-- A consumer coroutine that takes output messages from our component IO
-- and sends them using the websocket
wsSender
  :: forall eff
   . WS.WebSocket
  -> CR.Consumer MC.Message (Aff (HA.HalogenEffects (dom :: DOM | eff))) Unit
wsSender socket = CR.consumer \msg -> do
  case msg of
    MC.PassedCards msgContents ->
      liftEff $ WS.sendString socket $ C.cardsToString msgContents
  pure Nothing

type MainEffects = HA.HalogenEffects (
  dom :: DOM,
  ajax :: AJAX,
  console :: CONSOLE,
  random :: RANDOM
)
main :: Eff MainEffects Unit
main = launchAff_ do
  res <- affjax $ defaultRequest { url = "./cards.json", method = Left GET }
  liftEff $ case C.stringToCards res.response of
    Left error -> do
      log $ "Error parsing data: " <> error
      pure unit
    Right cards -> do
      pack <- take 15 <$> shuffle cards
      connection <- WS.create (WS.URL "ws://echo.websocket.org") []
      HA.runHalogenAff do
        body <- HA.awaitBody
        io <- runUI MC.component unit body

        -- Send the inital cards
        io.query $ MC.ReceiveCards pack unit

        -- The wsSender consumer subscribes to all output messages
        -- from our component
        io.subscribe $ wsSender connection

        -- Connecting the consumer to the producer initializes both,
        -- feeding queries back to our component as messages are received.
        CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query)