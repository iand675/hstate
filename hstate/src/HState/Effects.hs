{-# LANGUAGE ConstraintKinds #-}
module HState.Effects
  ( 
  -- * State machine lifecycle functions
    initializeE
  , transitionE
  , terminateE
  -- * The effect registry
  , EffectRegistry
  , Event(..)
  , emptyEffectRegistry
  , onEnter
  , EnterEvent
  , onExit
  , ExitEvent
  -- * Constraints
  , ValidInitialEvent
  , ValidTerminalEvent
  , ValidTransitionEvent
  ) where

import HState.Core
import HState.Internal
import Data.Kind
import Data.Proxy
import Data.Singletons
import Data.Typeable
import qualified Data.TypeRepMap as T

data Event (direction :: Direction) (schema :: Schema states events) (state :: states) (validity :: Validity) where
  -- | An event that is fired for the provided state when the state machine is initialized.
  Initialize :: Event 'Enter schema state (EventValidityForState state (SchemaInitialStates schema))
  -- | An event that is fired for the provided state when the state machine moves from one state to another.
  Transition :: Sing (event :: events) -> Event direction schema state (EventTriggersForTransition direction state (SchemaValidTransitions schema) event)
  -- | An event that is fired for the provided state when the state machine is terminated.
  Terminate :: Event 'Exit schema state (EventValidityForState state (SchemaEndStates schema))

newtype Contextualize (direction :: Direction) schema m context state = Contextualize
  { callback :: Event direction schema state 'Valid -> context state -> m (context state)
  }

instance Monad m => Semigroup (Contextualize direction schema m context state) where
  (Contextualize c1) <> (Contextualize c2) = Contextualize $ \e c -> do
    c1 e c >>= c2 e

instance Monad m => Monoid (Contextualize direction schema m context state) where
  mempty = Contextualize $ \_ c -> pure c

newtype Actions direction schema m context = Actions (T.TypeRepMap (Contextualize direction schema m context))

instance Monad m => Semigroup (Actions direction schema m context) where
  (Actions x) <> (Actions y) = Actions $ T.unionWith (<>) x y

instance Monad m => Monoid (Actions direction schema m context) where
  mempty = Actions T.empty

empty :: Actions direction schema m context
empty = Actions T.empty

insertAction :: forall direction schema m context thisState proxy.
    ( Monad m
    , Typeable thisState
    , KindOf thisState ~ SchemaStateType schema
    )
  => proxy schema
  -> Actions direction schema m context
  -> (Event direction schema thisState 'Valid -> context thisState -> m (context thisState))
  -> Actions direction schema m context
insertAction _schema (Actions m) f =
  Actions $
  T.alter go m
  where
    go Nothing = Just (Contextualize f)
    go (Just (Contextualize g)) = Just $ Contextualize $ \e c -> do
      c' <- g e c
      f e c'

lookupAction
  :: (KindOf state ~ SchemaStateType schema, Typeable state)
  => Actions direction schema m context
  -> proxy state
  -> Maybe (Event direction schema state 'Valid -> context state -> m (context state))
lookupAction (Actions m) _st = callback <$> T.lookup m

-- | Contains calbacks triggered during the lifecycle of the state machine.
--
--  The callbacks are executed sequentially in the order they are added via the 'onEnter' and 'onExit' functions.
data EffectRegistry (schema :: Schema states events) (m :: Type -> Type) (context :: states -> Type) = EffectRegistry
  { exitActions :: Actions 'Exit schema m context
  , enterActions :: Actions 'Enter schema m context
  }

-- | Merge two effect registries. Callbacks in the left registry are executed first, then the callbacks in the right one.
instance Monad m => Semigroup (EffectRegistry schema m context) where
  (EffectRegistry enter1 exit1) <> (EffectRegistry enter2 exit2) = EffectRegistry (enter1 <> enter2) (exit1 <> exit2)

instance Monad m => Monoid (EffectRegistry schema m context) where
  mempty = EffectRegistry mempty mempty

emptyEffectRegistry :: EffectRegistry schema m context
emptyEffectRegistry = EffectRegistry
  { exitActions = empty
  , enterActions = empty
  }

type ExitEvent schema state = Event 'Exit schema state 'Valid

-- | Add a callback to the 'EventRegistry' that triggers whenever the state is exited.
--
-- This occurs when the state machine transitions to another state, or when the state machine is terminated with the 'terminateE' function.
onExit
  :: forall state events m context schema.
    ( Monad m
    , SchemaStateType schema ~ KindOf state
    , SchemaEventType schema ~ events
    , Typeable state
    )
  => (ExitEvent schema state -> context state -> m (context state))
  -> EffectRegistry schema m context
  -> EffectRegistry schema m context
onExit f registry = registry
  { exitActions = insertAction (Proxy @schema) (exitActions registry) f
  }

type EnterEvent schema state = Event 'Enter schema state 'Valid

-- | Add a callback to the 'EventRegistry' that triggers whenever the state is entered.
--
-- This occurs when the state machine transitions from a previous state to the provided state type, or when the state machine is initialized via 'initializeE'.
onEnter
  :: forall state events m context schema.
    ( Monad m
    , SchemaStateType schema ~ KindOf state
    , SchemaEventType schema ~ events
    , Typeable state
    )
  => (EnterEvent schema state -> context state -> m (context state))
  -> EffectRegistry schema m context
  -> EffectRegistry schema m context
onEnter f registry = registry
  { enterActions = insertAction (Proxy @schema) (enterActions registry) f
  }

type ValidInitialEvent schema state =
  ( EventValidityForState state (SchemaInitialStates schema) ~ 'Valid
  , AllTransitionsAreTypeableFrom state (SchemaValidTransitions schema)
  , Typeable state
  , SchemaStateType schema ~ KindOf state
  )

initializeE ::
     ( Monad m
     , ValidInitialEvent schema currentState
     )
  => proxy schema
  -> EffectRegistry schema m context
  -> proxy' currentState
  -> context currentState
  -> m (Machine schema currentState context)
initializeE schema effectRegistry startingState ctxt= do
  let machine = initialize schema startingState ctxt

  case lookupAction (enterActions effectRegistry) startingState of
    Nothing -> do
      pure machine
    Just enterCallback -> do
      context' <- enterCallback Initialize ctxt
      pure $ setContext machine context'

type ValidTransitionEvent schema state nextState (event :: ev) =
  ( nextState ~ NewState state event (SchemaValidTransitions schema)
  , AllTransitionsAreTypeableFrom nextState (SchemaValidTransitions schema)
  , EventTriggersForTransition 'Exit state (SchemaValidTransitions schema) event ~ 'Valid
  , EventTriggersForTransition 'Enter nextState (SchemaValidTransitions schema) event ~ 'Valid
  , Typeable state
  , Typeable nextState
  , SchemaStateType schema ~ KindOf state
  , SchemaEventType schema ~ ev
  , Demote ev ~ ev
  , SingKind ev
  , SingI nextState
  )

-- | Transition the state machine to a new state.
--
-- The transition is only able to be performed if the provided event triggers a valid transition.
--
-- Upon successful transition, the state machine will execute the 'onExit' callbacks for the current state, 
-- then the 'onEnter' callbacks for the new state.
--
-- Finally, the context transformation function supplied to the 'transitionE' function is executed.
transitionE :: forall m ev schema currentState (event :: ev) nextState context.
    ( Monad m
    , ValidTransitionEvent schema currentState nextState event
    )
  => Machine schema currentState context -- ^ The pure state machine to transition.
  -> EffectRegistry schema m context -- ^ The effect registry to use.
  -> Sing event -- ^ The event that is triggering the transition.
  -> (context currentState -> m (context nextState)) -- ^ This callback is executed after the state machine has transitioned to the new state.
  -> m (Machine schema nextState context) -- ^ The updated state machine.
transitionE machine effectRegistry sEvent f = do

  postExitHookMachine <- case lookupAction (exitActions effectRegistry) (Proxy @currentState) of
    Nothing -> do
      pure machine
    Just exitCallback -> do
      context' <- exitCallback (Transition sEvent) $ getContext machine
      pure $ setContext machine context'

  postTransitionMachine <- transitionF postExitHookMachine sEvent f

  case lookupAction (enterActions effectRegistry) (Proxy @nextState) of
    Nothing -> do
      pure postTransitionMachine
    Just enterCallback -> do
      context' <- enterCallback (Transition sEvent) $ getContext postTransitionMachine
      pure $ setContext postTransitionMachine context'

type ValidTerminalEvent schema state context =
  ( Typeable state
  , SingI state
  , EventValidityForState state (SchemaEndStates schema) ~ 'Valid
  , SchemaStateType schema ~ KindOf state
  )

-- | Call provided 'onExit' callbacks for the current state, returning the current context.
terminateE ::
     ( Monad m
     , ValidTerminalEvent schema currentState context
     )
  => Machine schema currentState context  -- ^ The machine to "terminate". Practically speaking, all of the meaningful termination activity comes from the 'EffectRegistry'
  -> EffectRegistry schema m context -- ^ Effects performed on exit should be registered here
  -> m (Sing currentState, context currentState)
terminateE machine effectRegistry = do
  case lookupAction (exitActions effectRegistry) Proxy of
    Nothing -> pure $ terminate machine
    Just exitCallback -> do
      let (st, context) = terminate machine
      context' <- exitCallback Terminate context
      pure (st, context')
