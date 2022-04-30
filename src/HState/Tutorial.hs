module HState.Tutorial where
import Data.Function
import Data.Functor.Const
import Data.Singletons
import Data.Singletons.TH
import HState.Core
import HState.Effects
import Data.Eq.Singletons
import Data.Ord.Singletons
import Prelude.Singletons hiding (Const)
import Text.Show.Singletons

$(singletons [d|

  data WakingMachineState = Awake | Asleep
    deriving (Eq, Ord)

  data WakingMachineEvent = WakesUp | FallsAsleep
    deriving (Eq, Ord)

  wakingMachineSchema :: Schema WakingMachineState WakingMachineEvent
  (Just wakingMachineSchema) = mkSchema [Awake] [Asleep]
    [ (Awake, FallsAsleep, Asleep)
    , (Asleep, WakesUp, Awake)
    ]

  |])

deriving instance Show WakingMachineState
deriving instance Show WakingMachineEvent

testMachine = initialize sWakingMachineSchema SAwake (Const ())

test :: EffectRegistry WakingMachineSchema IO (Const ())
test = 
  emptyEffectRegistry @WakingMachineSchema
    & onExit @'Awake (\ev ctx -> putStrLn "Bye 1" >> pure ctx)
    & onExit @'Awake (\ev ctx -> putStrLn "Bye 2" >> pure ctx)