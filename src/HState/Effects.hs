module HState.Effects 
  ( EffectRegistry
  , emptyEffectRegistry
  , onEnter
  , onExit
  , transitionE
  ) where

import Control.Monad.IO.Class
import GHC.Exts
import HState.Core
import HState.Internal
import Data.Function
import Data.Functor.Const
import Data.Kind
import Data.Proxy
import Data.Singletons
import Data.Typeable
import qualified Data.TypeRepMap as T

data Event e
  = Initialize
  | Transition e
  | Terminate

data Contextualize m context events states state = Contextualize
  { callback :: Event events -> context state -> m (context state)
  }

instance (Typeable state) => Show (Contextualize m context events states state) where
  show x = "Contextualize#" ++ show (typeRep x)

-- TODO: TypeRepMap (Const callbackTypeThing)
newtype Actions m context events states = Actions (T.TypeRepMap (Contextualize m context events states))
  deriving (Show)

empty :: Actions m context events states
empty = Actions T.empty

insertAction :: forall schema m context event state thisState proxy. 
    ( Monad m
    , Typeable thisState
    , KindOf thisState ~ state
    , SchemaStateType schema ~ state
    , SchemaEventType schema ~ event
    )
  => proxy schema
  -> Actions m context event state
  -> (Event event -> context thisState -> m (context thisState)) 
  -> Actions m context event state
insertAction schema (Actions m) f = 
  Actions $ 
  T.alter go m
  where
    go Nothing = Just (Contextualize f)
    go (Just (Contextualize g)) = Just $ Contextualize $ \e c -> do
      c' <- g e c
      f e c'

lookupAction 
  :: (KindOf state ~ states, Typeable state) 
  => Actions m context events states 
  -> proxy state 
  -> Maybe (Event events -> context state -> m (context state))
lookupAction (Actions m) st = fmap callback $ T.lookup m

data EffectRegistry (schema :: Schema states events) (m :: Type -> Type) (context :: states -> Type) = EffectRegistry
  { exitActions :: Actions m context (SchemaEventType schema) (SchemaStateType schema)
  , enterActions :: Actions m context (SchemaEventType schema) (SchemaStateType schema)
  } deriving (Show)

emptyEffectRegistry :: EffectRegistry schema m context
emptyEffectRegistry = EffectRegistry
  { exitActions = empty
  , enterActions = empty
  }

onExit 
  :: forall state events m context schema proxy.
    ( Monad m
    , SchemaStateType schema ~ KindOf state
    , SchemaEventType schema ~ events
    , Typeable state
    )
  => (Event events -> context state -> m (context state))
  -> EffectRegistry schema m context
  -> EffectRegistry schema m context
onExit f registry = registry 
  { exitActions = insertAction (Proxy @schema) (exitActions registry) f
  }

onEnter 
  :: forall state events m context schema proxy.
    ( Monad m
    , SchemaStateType schema ~ KindOf state
    , SchemaEventType schema ~ events
    , Typeable state
    )
  => (Event events -> context state -> m (context state))
  -> EffectRegistry schema m context
  -> EffectRegistry schema m context
onEnter f registry = registry 
  { enterActions = insertAction (Proxy @schema) (enterActions registry) f
  }

-- initializeE ::
--      ( Monad m
--      , schema ~ 'Schema initial terminal transitions
--      , ValidState Initial currentState initial
--      )
--   => Sing schema
--   -> EffectRegistry schema m context
--   -> Sing currentState
--   -> context currentState
--   -> m (Machine schema currentState context)
-- initializeE schema effectRegistry startingState ctxt= do
--   let machine = initialize schema startingState ctxt

--   case lookupAction (enterActions effectRegistry) startingState of
--     Nothing -> do
--       pure machine
--     Just onEnter -> do
--       context' <- onEnter Initialize ctxt
--       pure $ setContext machine context'

transitionE :: forall m ev schema currentState (event :: ev) nextState context. 
    ( Monad m
    , Typeable currentState
    , Typeable nextState
    , SingI nextState
    , SingKind ev
    , Demote ev ~ ev
    , SchemaStateType schema ~ KindOf currentState
    , SchemaEventType schema ~ ev
    , nextState ~ NewState currentState event (SchemaValidTransitions schema)
    )
  => Machine schema currentState context 
  -> EffectRegistry schema m context
  -> Sing event
  -> (context currentState -> context nextState)
  -> m (Machine schema nextState context)
transitionE machine effectRegistry sEvent f = do
  let event = Transition (fromSing sEvent :: ev)

  postExitHookMachine <- case lookupAction (exitActions effectRegistry) (Proxy @currentState) of
    Nothing -> do
      pure machine
    Just onExit -> do
      context' <- onExit event $ getContext machine
      pure $ setContext machine context'

  let postTransitionMachine = transition postExitHookMachine sEvent f

  case lookupAction (enterActions effectRegistry) (Proxy @nextState) of
    Nothing -> do
      pure postTransitionMachine
    Just onEnter -> do
      context' <- onEnter event $ getContext postTransitionMachine
      pure $ setContext postTransitionMachine context'

-- terminateE ::
--      (Monad m)
--   => Machine schema currentState context 
--   -> EffectRegistry schema m context
--   -> m (Sing currentState, context currentState)
-- terminateE machine effectRegistry = do
--   case lookupAction (exitActions effectRegistry) (Proxy @currentState) of
--     Nothing -> do
--       pure (getState machine, getContext machine)
--     Just onExit -> do
--       context' <- onExit (getEvent machine) $ getContext machine
--       pure (getState machine, context')

registryFor :: forall schema m context. EffectRegistry schema m context
registryFor = emptyEffectRegistry

test :: EffectRegistry WakingMachineSchema IO (Const ())
test = 
  registryFor @WakingMachineSchema
    & onExit @'Awake (\ev ctx -> putStrLn "Bye 1" >> pure ctx)
    & onExit @'Awake (\ev ctx -> putStrLn "Bye 2" >> pure ctx)