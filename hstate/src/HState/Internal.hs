{-# LANGUAGE MultiParamTypeClasses #-}
module HState.Internal where
import Data.Singletons.TH
import Data.Kind
import Control.Monad (guard)
import Control.Monad.Singletons
import Data.Eq.Singletons
import Data.Ord.Singletons
import GHC.TypeLits
import Text.Show.Singletons
import Data.List (nub)
import Data.List.Singletons
import Prelude.Singletons

$(singletons [d|
  data Schema state event = Schema
    { schemaInitialStates :: [state]
    , schemaEndStates :: [state]
    , schemaValidTransitions :: [(state, event, state)]
    } deriving (Show, Eq)

  mkSchema 
    :: (Eq state, Eq event) 
    => [state] 
    -> [state] 
    -> [(state, event, state)] 
    -> Maybe (Schema state event)
  mkSchema initial end transitions = do
    guard $ not $ null initial
    pure $ Schema
      { schemaInitialStates = nub initial
      , schemaEndStates = nub end
      , schemaValidTransitions = nub transitions
      }

  data BasicMachine state event = BasicMachine
    { machineSchema :: Schema state event
    , machineCurrentState :: state
    }

  data Direction = Enter | Exit
  data Validity = Valid | Invalid

  eventTriggersForTransition :: (Eq state, Eq event) => Direction -> state -> [(state, event, state)] -> event -> Validity
  eventTriggersForTransition Exit s ts e = if any (\(st', e', _) -> s == st' && e == e') ts 
    then Valid else Invalid
  eventTriggersForTransition Enter s ts e = if any (\(_, e', st') -> s == st' && e == e') ts 
    then Valid else Invalid

  |])

data Initial
data Terminal

type family NewState (currentState :: st) (event :: e) (transitions :: [(st, e, st)]) where
  NewState currentState event '[] = TypeError ('Text "Invalid transition")
  NewState currentState event ('(currentState, event, newState) ': _) = newState
  NewState currentState event (_ ': transitions) = NewState currentState event transitions

type family ValidState validatedAgainst (s :: k) (ss :: [k]) :: Constraint where
  ValidState validatedAgainst s '[] = TypeError ('Text "Invalid " ':<>: 'ShowType validatedAgainst ':<>: 'Text " state: " ':<>: 'ShowType s)
  ValidState _ s (s ': _) = ()
  ValidState validatedAgainst s (s' ': ss) = ValidState validatedAgainst s ss

type family SchemaEventType (s :: Schema state event) where
  SchemaEventType ('Schema initial terminal transitions) = EventTypeFromTransitions transitions

type family EventTypeFromTransitions (transitions :: [(initial, event, terminal)]) where
  EventTypeFromTransitions '[] = TypeError ('Text "No transitions")
  EventTypeFromTransitions ('(_, event, _) ': _) = KindOf event

type family TypeFromList (l :: [k]) where
  TypeFromList (x ': _) = KindOf x
  TypeFromList '[] = TypeError ('Text "Empty list")

type family SchemaStateType (s :: Schema state event) where
  SchemaStateType ('Schema initial _ _) = TypeFromList initial

type family EventValidityForState (s :: st) (ss :: [st]) :: Validity where
  EventValidityForState s (s ': ss) = 'Valid
  EventValidityForState s (s' ': ss) = EventValidityForState s ss
  EventValidityForState s '[] = 'Invalid

type family InnerType (f :: k -> Type) where
  InnerType (f a) = a

mkBasicMachine 
  :: (ValidState Initial state initialStates)
  => proxy ('Schema initialStates endStates validTransitions)
  -> proxy' state
  -> Proxy ('BasicMachine
            ('Schema initialStates endStates validTransitions)
            state
           )
mkBasicMachine _ _ = Proxy

transition_ 
  :: ( nextState ~ NewState currentState event (SchemaValidTransitions schema)
     , SingI nextState
     )
  => Sing ('BasicMachine
            schema
            currentState
          )
  -> Sing event 
  -> Sing ('BasicMachine
            schema
            nextState
          )
transition_ (SBasicMachine schema _) _ = SBasicMachine schema sing
-- action
-- context
-- state
-- parallel machines
-- hierarchical state machines