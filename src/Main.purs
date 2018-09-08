module Main where

import Prelude

import ChocoPie (runChocoPie)
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Class.Console (error)
import Effect.Console (log)
import FRP.Event as E
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.Events as Events
import Record as Record
import ReduxDevTools as DT
import Simple.JSON as JSON
import Type.Prelude (SProxy(..))

type State = { counter :: Int }

initState :: State
initState = { counter: 1 }

type Action = Variant.Variant
  ( add :: Int
  , sub :: Int
  , init :: {}
  )

counterS = SProxy :: SProxy "counter"
initS = SProxy :: SProxy "init"
addS = SProxy :: SProxy "add"
subS = SProxy :: SProxy "sub"

init_ :: Action
init_ = Variant.inj initS {}

add_ :: Int -> Action
add_ = Variant.inj addS

sub_ :: Int -> Action
sub_ = Variant.inj subS

update :: Action -> State -> State
update action s = Variant.match
  { add: \x -> Record.modify counterS (add x) s
  , sub: \x -> Record.modify counterS (flip sub x) s
  , init: \_ -> s
  } action

type Pair =
  { state :: State
  , action :: Maybe Action
  }

mkSinks
  :: { render :: E.Event Action
     , devTools :: E.Event Action
     }
  -> { render :: E.Event State
     , devTools :: E.Event Pair
     }
mkSinks sources =
  { render: states
  , devTools: pairs
  }
  where
    actions = pure init_ <|> sources.render <|> sources.devTools
    pairs = E.fold mkPair actions { state: initState, action: Nothing }
    states = _.state <$> pairs
    mkPair action pair = pair
      { state = update action pair.state
      , action = Just action
      }

foreign import render :: RB.JSX -> Effect Unit

renderDriver :: E.Event State -> Effect (E.Event Action)
renderDriver state = do
  {event, push} <- E.create
  _ <- E.subscribe (view push <$> state) render
  pure event
  where
    view push props = R.div_
      [ R.h1_ [R.text $ show props.counter]
      , R.button
          { children: pure $ R.text "+1"
          , onClick: Events.handler_ do
              push (add_ 1)
          }
      , R.button
          { children: pure $ R.text "-1"
          , onClick: Events.handler_ do
              push (sub_ 1)
          }
      ]

devToolsDriver :: E.Event Pair -> Effect (E.Event Action)
devToolsDriver pairs = do
  {event, push} <- E.create
  mExt <- DT.getExtension
  case mExt of
    Just ext -> do
      log "Found extension"
      -- make instance
      inst <- DT.connect ext (DT.mkConnectOptions {})
      -- listen and just print out what we get from DevTools
      DT.subscribe inst (log <<< JSON.writeJSON)
      _ <- E.subscribe pairs (sendUpdates inst)
      log "Subscribed to updates"
    Nothing -> do
      error "No extension found"
  pure event
  where
    sendUpdates inst {state, action} = case action of
      Just x -> do
        DT.send inst {state: JSON.write state, action: JSON.write x}
      Nothing -> do
        -- feed initial state to DevTools
        DT.init inst (JSON.write state)


main :: Effect Unit
main = do
  runChocoPie mkSinks
    { devTools: devToolsDriver
    , render: renderDriver
    }
