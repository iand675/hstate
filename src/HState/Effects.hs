{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
module HState.Effects 
  ( EffectRegistry
  , Event(..)
  , emptyEffectRegistry
  , onEnter
  , onExit
  , initializeE
  , transitionE
  , terminateE
  ) where

import HState.Core
import HState.Internal
import Data.Kind
import Data.Proxy
import Data.Singletons
import Data.Typeable
import qualified Data.TypeRepMap as T

data Event (direction :: Direction) (schema :: Schema states events) (state :: states) (validity :: Validity) where
  Initialize :: Event 'Enter schema state (EventValidityForState state (SchemaInitialStates schema))
  Transition :: Sing (event :: events) -> Event direction schema state (EventTriggersForTransition direction state (SchemaValidTransitions schema) event)
  Terminate :: Event 'Exit schema state (EventValidityForState state (SchemaEndStates schema))

data Contextualize (direction :: Direction) schema m context state = Contextualize
  { callback :: Event direction schema state 'Valid -> context state -> m (context state)
  }

instance Monad m => Semigroup (Contextualize direction schema m context state) where
  (Contextualize c1) <> (Contextualize c2) = Contextualize $ \e c -> do
    c1 e c >>= c2 e

instance Monad m => Monoid (Contextualize direction schema m context state) where
  mempty = Contextualize $ \_ c -> pure c

instance (Typeable state) => Show (Contextualize direction schema m context state) where
  show x = "Contextualize#" ++ show (typeRep x)

newtype Actions direction schema m context = Actions (T.TypeRepMap (Contextualize direction schema m context))
  deriving (Show)

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
lookupAction (Actions m) _st = fmap callback $ T.lookup m

data EffectRegistry (schema :: Schema states events) (m :: Type -> Type) (context :: states -> Type) = EffectRegistry
  { exitActions :: Actions 'Exit schema m context
  , enterActions :: Actions 'Enter schema m context
  } deriving (Show)

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

initializeE ::
     ( Monad m
     , EventValidityForState currentState (SchemaInitialStates schema) ~ 'Valid 
     , AllTransitionsAreTypeableFrom currentState (SchemaValidTransitions schema)
     , Typeable currentState
     , SchemaStateType schema ~ KindOf currentState
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
    , AllTransitionsAreTypeableFrom nextState (SchemaValidTransitions schema)
    , EventTriggersForTransition 'Exit currentState (SchemaValidTransitions schema) event ~ 'Valid
    , EventTriggersForTransition 'Enter nextState (SchemaValidTransitions schema) event ~ 'Valid
    )
  => Machine schema currentState context
  -> EffectRegistry schema m context
  -> Sing event
  -> (context currentState -> m (context nextState))
  -> m (Machine schema nextState context)
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

terminateE ::
     ( Monad m
     , Typeable currentState
     , SingI currentState
     , EventValidityForState currentState (SchemaEndStates schema) ~ 'Valid
     , SchemaStateType schema ~ KindOf currentState
     )
  => Machine schema currentState context 
  -> EffectRegistry schema m context
  -> m (Sing currentState, context currentState)
terminateE machine effectRegistry = do
  case lookupAction (exitActions effectRegistry) Proxy of
    Nothing -> pure $ terminate machine
    Just exitCallback -> do
      let (st, context) = terminate machine
      context' <- exitCallback Terminate context
      pure (st, context')
