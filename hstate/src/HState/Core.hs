{-# LANGUAGE NamedFieldPuns #-}
module HState.Core
  ( Schema(..)
  , mkSchema
  , sMkSchema
  , MkSchema
  , Machine
  , MachineInAnyState(..)
  , currentStateI
  , AllTransitionsAreTypeableFrom
  , initialize
  , suspend
  , resume
  , HState.Core.transition
  , transitionF
  , terminate
  , getContext
  , setContext
  , currentState
  , modifyContext
  -- * Re-exports
  , MkSchemaSym0
  ) where
import HState.Internal
import Data.Kind
import Data.Maybe.Singletons
import Data.Singletons
import Data.Typeable

data MachineInAnyState (s :: Schema state event) (c :: state -> Type) where
  MachineInAnyState :: forall s c currentState. SingI currentState => Machine s currentState c -> MachineInAnyState s c

currentStateI :: SingI (currentState) => Machine schema currentState context -> Sing currentState
currentStateI _ = sing

type family AllTransitionsAreTypeableFrom (currentState :: states) (transitions :: [(states, events, states)]) :: Constraint where
  AllTransitionsAreTypeableFrom currentState ('(currentState, _, nextState) ': transitions) = (Typeable nextState, AllTransitionsAreTypeableFrom currentState transitions)
  AllTransitionsAreTypeableFrom currentState ('(_, _, _) ': transitions) = AllTransitionsAreTypeableFrom currentState transitions
  AllTransitionsAreTypeableFrom _ '[] = ()

data Machine (schema :: Schema states events) (currentState :: states) (context :: states -> Type) where
  Machine :: (AllTransitionsAreTypeableFrom currentState (SchemaValidTransitions schema)) => 
    { context :: context currentState } -> Machine schema currentState context

initialize 
  :: ( EventValidityForState currentState (SchemaInitialStates schema) ~ 'Valid
     , AllTransitionsAreTypeableFrom currentState (SchemaValidTransitions schema)
     )
  => proxy schema
  -> proxy' currentState
  -> context currentState 
  -> Machine schema currentState context
initialize _ _startingState ctxt = 
  Machine
    { context = ctxt
    }

suspend 
  :: (SingI currentState)
  => Machine schema currentState context 
  -> (Sing currentState, context currentState)
suspend Machine{context} = (sing, context)

resume
  :: AllTransitionsAreTypeableFrom currentState (SchemaValidTransitions schema)
  => proxy schema
  -> proxy' currentState
  -> context currentState
  -> Machine schema currentState context
resume _ _ ctxt = Machine ctxt

transition
  :: ( AllTransitionsAreTypeableFrom nextState (SchemaValidTransitions schema)
     , NewState currentState event (SchemaValidTransitions schema) ~ nextState
     )
  => Machine schema currentState context
  -> proxy event
  -> (context currentState -> context nextState)
  -> Machine schema nextState context
transition (Machine context) _event f = Machine $ f context

transitionF
  :: ( AllTransitionsAreTypeableFrom nextState (SchemaValidTransitions schema)
     , Functor f
     , NewState currentState event (SchemaValidTransitions schema) ~ nextState
     )
  => Machine schema currentState context
  -> proxy event
  -> (context currentState -> f (context nextState))
  -> f (Machine schema nextState context)
transitionF Machine{context} _event f = (\context' -> Machine context') <$> f context
    
terminate 
  :: ( EventValidityForState currentState (SchemaEndStates schema) ~ 'Valid
     , SingI currentState
     )
  => Machine schema currentState context
  -> (Sing currentState, context currentState)
terminate Machine{context} = (sing, context)

currentState :: SingI currentState => Machine schema currentState context -> Sing currentState
currentState _ = sing

getContext :: Machine schema currentState context -> context currentState
getContext = context

modifyContext :: Machine schema currentState context -> (context currentState -> context currentState) -> Machine schema currentState context
modifyContext m@Machine{context} f = m
  { context = f context
  }

setContext :: Machine schema currentState context -> context currentState -> Machine schema currentState context
setContext m ctxt = m
  { context = ctxt
  }

-- Invoked effects, which execute a side-effect that can send and receive events asynchronously:

-- "Fire-and-forget" effects, which execute a synchronous side-effect with no events sent back to the statechart, or send an event synchronously back to the statechart: