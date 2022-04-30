{-# LANGUAGE NamedFieldPuns #-}
module HState.Core
  ( Schema(..)
  , mkSchema
  , MkSchema
  , Machine
  , initialize
  , suspend
  , HState.Core.transition
  , terminate
  , getContext
  , setContext
  , modifyContext
  , testMachine
  ) where
import HState.Internal
import Data.Functor.Const
import Data.Kind
import Data.Maybe.Singletons
import Data.Eq.Singletons
import Data.Singletons

testMachine = initialize sWakingMachineSchema SAwake (Const ())

data Machine (schema :: Schema states events) (currentState :: states) (context :: states -> Type) where 
  Machine ::
    { internalMachine :: Sing 
        ('BasicMachine 
          schema
          currentState
        )
    , context :: context currentState
    } -> Machine schema currentState context

initialize 
  :: ( schema ~ 'Schema initial terminal transitions
     , ValidState Initial currentState initial
     )
  => Sing schema
  -> Sing currentState 
  -> context currentState 
  -> Machine schema currentState context
initialize schema startingState ctxt = 
  Machine
    { internalMachine = mkBasicMachine schema startingState
    , context = ctxt
    }

suspend 
  :: (SingKind (KindOf currentState))
  => Machine schema currentState context 
  -> (Sing currentState, context currentState)
suspend Machine{context, internalMachine = SBasicMachine _ st} = (st, context)

resume
  :: Sing schema
  -> Sing currentState
  -> context currentState
  -> Machine schema currentState context
resume schema st ctxt = Machine
  { internalMachine = SBasicMachine schema st
  , context = ctxt
  }

transition
  :: ( NewState currentState event (SchemaValidTransitions schema) ~ nextState
     , SingI nextState
     )
  => Machine schema currentState context
  -> Sing event
  -> (context currentState -> context nextState)
  -> Machine schema nextState context
transition m@Machine{internalMachine, context} event f = 
  m { internalMachine = transition_ internalMachine event
    , context = f context
    }
    
terminate 
  :: ( ValidState Terminal currentState terminal
     , schema ~ 'Schema initial terminal transitions
     )
  => Machine schema currentState context
  -> (Sing currentState, context currentState)
terminate Machine{context, internalMachine = SBasicMachine _ st} = (st, context)

currentState :: Machine schema currentState context -> Sing currentState
currentState Machine{internalMachine = SBasicMachine _ st} = st

getContext :: Machine schema currentState context -> context currentState
getContext = context

modifyContext :: Machine schema currentState context -> (context currentState -> context currentState) -> Machine schema currentState context
modifyContext m@Machine{context} f = m
  { context = f context
  }

setContext :: Machine schema currentState context -> context currentState -> Machine schema currentState context
setContext m@Machine{context} ctxt = m
  { context = ctxt
  }

-- Invoked effects, which execute a side-effect that can send and receive events asynchronously:

-- "Fire-and-forget" effects, which execute a synchronous side-effect with no events sent back to the statechart, or send an event synchronously back to the statechart: