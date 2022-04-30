{-# LANGUAGE NamedFieldPuns #-}
module HState.Core
  ( Schema(..)
  , mkSchema
  , sMkSchema
  , MkSchema
  , Machine
  , initialize
  , suspend
  , HState.Core.transition
  , transitionF
  , terminate
  , getContext
  , setContext
  , modifyContext
  -- * Re-exports
  , MkSchemaSym0
  ) where
import HState.Internal
import Data.Functor.Const
import Data.Kind
import Data.Maybe.Singletons
import Data.Eq.Singletons
import Data.Singletons


newtype Machine (schema :: Schema states events) (currentState :: states) (context :: states -> Type) where 
  Machine ::
    { context :: context currentState
    } -> Machine schema currentState context

initialize 
  :: ( ValidState Initial currentState (SchemaInitialStates schema)
     )
  => proxy schema
  -> proxy' currentState
  -> context currentState 
  -> Machine schema currentState context
initialize schema startingState ctxt = 
  Machine
    { context = ctxt
    }

suspend 
  :: (SingI currentState)
  => Machine schema currentState context 
  -> (Sing currentState, context currentState)
suspend Machine{context} = (sing, context)

resume
  :: proxy schema
  -> proxy' currentState
  -> context currentState
  -> Machine schema currentState context
resume schema st ctxt = Machine
  { context = ctxt
  }

transition
  :: ( NewState currentState event (SchemaValidTransitions schema) ~ nextState
     )
  => Machine schema currentState context
  -> proxy event
  -> (context currentState -> context nextState)
  -> Machine schema nextState context
transition m@Machine{context} event f = 
  m { context = f context
    }
    
transitionF
  :: ( Functor f
     , NewState currentState event (SchemaValidTransitions schema) ~ nextState
     )
  => Machine schema currentState context
  -> proxy event
  -> (context currentState -> f (context nextState))
  -> f (Machine schema nextState context)
transitionF m@Machine{context} event f = (\context' -> m { context = context' }) <$> f context
    
terminate 
  :: ( ValidState Terminal currentState (SchemaEndStates schema)
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
setContext m@Machine{context} ctxt = m
  { context = ctxt
  }

-- Invoked effects, which execute a side-effect that can send and receive events asynchronously:

-- "Fire-and-forget" effects, which execute a synchronous side-effect with no events sent back to the statechart, or send an event synchronously back to the statechart: