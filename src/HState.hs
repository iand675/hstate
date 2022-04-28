{-# LANGUAGE NamedFieldPuns #-}
module HState 
  ( Schema(..)
  , mkSchema
  , MkSchema
  , Machine
  , initialize
  , suspend
  , HState.transition
  , terminate
  ) where
import HState.Internal
import Data.Maybe.Singletons
import Data.Eq.Singletons
import Data.Singletons

testMachine = initialize sWakingMachineSchema SAwake

data Machine (initial :: [st]) (terminal :: [st]) (transitions :: [(st, ev, st)]) (currentState :: st) context where 
  Machine ::
    { internalMachine :: Sing 
        ('BasicMachine 
          ('Schema initial terminal transitions)
          currentState
        )
    , context :: context
    } -> Machine initial terminal transitions currentState context

initialize 
  :: (ValidState Initial currentState initial)
  => Sing ('Schema initial terminal transitions) 
  -> Sing currentState 
  -> context 
  -> Machine initial terminal transitions currentState context
initialize schema startingState ctxt = 
  Machine
    { internalMachine = mkBasicMachine schema startingState
    , context = ctxt
    }

suspend 
  :: (SingKind (KindOf currentState))
  => Machine initial terminal transitions currentState context 
  -> (Sing currentState, context)
suspend Machine{context, internalMachine = SBasicMachine _ st} = (st, context)

resume
  :: Sing ('Schema initial terminal transitions) 
  -> Sing currentState
  -> context
  -> Machine initial terminal transitions currentState context
resume schema st ctxt = Machine
  { internalMachine = SBasicMachine schema st
  , context = ctxt
  }

transition
  :: (Transition ('BasicMachine ('Schema initial terminal transitions) currentState) ev ~ 'Just ('BasicMachine ('Schema initial terminal transitions) nextState), SEq (KindOf currentState), SEq (KindOf ev))
  => Machine initial terminal transitions currentState context
  -> Sing ev
  -> Machine initial terminal transitions nextState context
transition Machine{internalMachine, context} event = 
  Machine
    { internalMachine = sFromJust $ sTransition internalMachine event
    , context = context
    }
    
terminate 
  :: (ValidState Terminal currentState terminal)
  => Machine initial terminal transitions currentState context
  -> (Sing currentState, context)
terminate Machine{context, internalMachine = SBasicMachine _ st} = (st, context)

currentState :: Machine initial terminal transitions currentState context -> Sing currentState
currentState Machine{internalMachine = SBasicMachine _ st} = st

-- Invoked effects, which execute a side-effect that can send and receive events asynchronously:

-- "Fire-and-forget" effects, which execute a synchronous side-effect with no events sent back to the statechart, or send an event synchronously back to the statechart: