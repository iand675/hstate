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
import Data.List (find, nub)
import Data.List.Singletons
import Data.Singletons.Base.Enum
import Prelude.Singletons
-- data a :- b = a :- b

-- data a :-> b = a :-> b

-- data Awake
-- data WakesUp = WakesUp
-- data FallsAsleep = FallsAsleep

-- type Schema =
--   [ Asleep :- WakesUp :> Awake
--   , Awake :- FallsAsleep :> Asleep
--   ]


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

  lookupTransition :: (Eq state, Eq event) => Schema state event -> state -> event -> Maybe state
  lookupTransition schema currentState event = do
    (_, _, newState) <- find (\(s, e, _) -> s == currentState && e == event) $ schemaValidTransitions schema
    pure newState 

  unsafeLookupTransition :: (Eq state, Eq event) => Schema state event -> state -> event -> state
  unsafeLookupTransition schema currentState event = do
    case lookupTransition schema currentState event of
      Nothing -> error "Invalid transition"
      Just s -> s

  data WakingMachineState = Awake | Asleep
    deriving (Show, Eq, Ord, Enum, Bounded)

  data WakingMachineEvent = WakesUp | FallsAsleep
    deriving (Show, Eq, Ord, Enum, Bounded)

  wakingMachineSchema :: Schema WakingMachineState WakingMachineEvent
  (Just wakingMachineSchema) = mkSchema [Awake] [Asleep]
    [ (Awake, FallsAsleep, Asleep)
    , (Asleep, WakesUp, Awake)
    ]

  data BasicMachine state event = BasicMachine
    { machineSchema :: Schema state event
    , machineCurrentState :: state
    }

  transition :: (Eq state, Eq event) => BasicMachine state event -> event -> Maybe (BasicMachine state event)
  transition m e = do
    newState <- lookupTransition (machineSchema m) (machineCurrentState m) e
    pure $ m { machineCurrentState = newState }
  |])

data Initial
data Terminal

type family ValidState validatedAgainst (s :: k) (ss :: [k]) :: Constraint where
  ValidState validatedAgainst s '[] = TypeError (Text "Invalid " :<>: ShowType validatedAgainst :<>: Text " state: " :<>: ShowType s)
  ValidState _ s (s ': _) = ()
  ValidState validatedAgainst s (s' ': ss) = ValidState validatedAgainst s ss

mkBasicMachine 
  :: (ValidState Initial state initialStates)
  => Sing ('Schema initialStates endStates validTransitions)
  -> Sing state
  -> Sing ('BasicMachine
            ('Schema initialStates endStates validTransitions)
            state
          )
mkBasicMachine = SBasicMachine

-- action
-- context
-- state
-- parallel machines
-- hierarchical state machines