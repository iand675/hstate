{-# LANGUAGE LambdaCase #-}
module HState.Tutorial where
import Control.Monad
import Data.Function
import Data.Functor.Const
import Data.Singletons
import Data.Singletons.TH
import HState.Core
import HState.Effects
import Data.Eq.Singletons
import Data.Ord.Singletons
import Prelude.Singletons hiding (Const)
import HState.Internal
import Data.Functor

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

testMachine :: Machine WakingMachineSchema 'Awake (Const ())
testMachine = initialize sWakingMachineSchema SAwake (Const ())

test :: EffectRegistry WakingMachineSchema IO (Const ())
test =
  emptyEffectRegistry @WakingMachineSchema
    & onEnter @'Awake (\_ev ctx -> putStrLn "Good morning" >> pure ctx)
    & onExit @'Awake (\_ev ctx -> putStrLn "Goodnight" >> pure ctx)
    & onEnter @'Asleep asleepEnter
    & onExit @'Asleep asleepExit
  where
    asleepExit :: Event 'Exit WakingMachineSchema 'Asleep 'Valid -> Const () 'Asleep -> IO (Const () 'Asleep)
    asleepExit ev ctx = do
      case ev of
        Terminate -> putStrLn "Wrapping up"
        Transition st -> case st of
          SWakesUp -> putStrLn "Waking up"

      pure ctx

    asleepEnter :: Event 'Enter WakingMachineSchema 'Asleep 'Valid -> Const () 'Asleep -> IO (Const () 'Asleep)
    asleepEnter ev ctx = do
      case ev of
        Transition st -> putStrLn "Falling asleep" $> ctx

sendWithoutKnowledge
  :: MachineInAnyState WakingMachineSchema (Const ())
  -> WakingMachineEvent
  -> IO (MachineInAnyState WakingMachineSchema (Const ()))
sendWithoutKnowledge (MachineInAnyState m) ev = withSomeSing ev $ \evI -> case currentStateI m of
  SAwake -> case evI of
    SWakesUp -> pure $ MachineInAnyState m
    SFallsAsleep -> do
      result <- transitionE m test SFallsAsleep (pure . Const . getConst)
      pure $ MachineInAnyState result
  SAsleep -> case evI of
    SWakesUp -> do
      result <- transitionE m test SWakesUp (pure . Const . getConst)
      pure $ MachineInAnyState result
    SFallsAsleep -> pure $ MachineInAnyState m

main :: IO ()
main = do
  machine <- initializeE sWakingMachineSchema test SAwake (Const ())
  runningMachine <- foldM
    sendWithoutKnowledge
    (MachineInAnyState machine :: MachineInAnyState WakingMachineSchema (Const ()))
    [WakesUp, FallsAsleep, WakesUp, FallsAsleep]
  case runningMachine of
    MachineInAnyState m -> case currentStateI m of
      SAwake -> putStrLn "I'm not done yet!"
      SAsleep -> do
        void $ terminateE m test
        putStrLn "Sleep the day away!"
