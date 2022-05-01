{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Control.Monad.IO.Class
import GHC.Exts
import GHC.Generics
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
  deriving (Show, Eq, Ord, Functor, Generic)

data Contextualize schema m context state = Contextualize
  { callback :: Event (SchemaEventType schema) -> context state -> m (context state)
  }

instance Monad m => Semigroup (Contextualize schema m context state) where
  (Contextualize c1) <> (Contextualize c2) = Contextualize $ \e c -> do
    c1 e c >>= c2 e

instance Monad m => Monoid (Contextualize schema m context state) where
  mempty = Contextualize $ \_ c -> pure c

instance (Typeable state) => Show (Contextualize schema m context state) where
  show x = "Contextualize#" ++ show (typeRep x)

newtype Actions schema m context = Actions (T.TypeRepMap (Contextualize schema m context))
  deriving (Show)

instance Monad m => Semigroup (Actions schema m context) where
  (Actions x) <> (Actions y) = Actions $ T.unionWith (<>) x y

instance Monad m => Monoid (Actions schema m context) where
  mempty = Actions T.empty

empty :: Actions schema m context
empty = Actions T.empty

insertAction :: forall schema m context thisState proxy. 
    ( Monad m
    , Typeable thisState
    , KindOf thisState ~ SchemaStateType schema
    )
  => proxy schema
  -> Actions schema m context
  -> (Event (SchemaEventType schema) -> context thisState -> m (context thisState)) 
  -> Actions schema m context
insertAction schema (Actions m) f = 
  Actions $ 
  T.alter go m
  where
    go Nothing = Just (Contextualize f)
    go (Just (Contextualize g)) = Just $ Contextualize $ \e c -> do
      c' <- g e c
      f e c'

lookupAction 
  :: (KindOf state ~ SchemaStateType schema, Typeable state) 
  => Actions schema m context
  -> proxy state 
  -> Maybe (Event (SchemaEventType schema) -> context state -> m (context state))
lookupAction (Actions m) st = fmap callback $ T.lookup m

data EffectRegistry (schema :: Schema states events) (m :: Type -> Type) (context :: states -> Type) = EffectRegistry
  { exitActions :: Actions schema m context
  , enterActions :: Actions schema m context
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

initializeE ::
     ( Monad m
     , ValidState Initial currentState (SchemaInitialStates schema)
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
    Just onEnter -> do
      context' <- onEnter Initialize ctxt
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
    )
  => Machine schema currentState context
  -> EffectRegistry schema m context
  -> Sing event
  -> (context currentState -> m (context nextState))
  -> m (Machine schema nextState context)
transitionE machine effectRegistry sEvent f = do
  let event = Transition (fromSing sEvent :: ev)

  postExitHookMachine <- case lookupAction (exitActions effectRegistry) (Proxy @currentState) of
    Nothing -> do
      pure machine
    Just onExit -> do
      context' <- onExit event $ getContext machine
      pure $ setContext machine context'

  postTransitionMachine <- transitionF postExitHookMachine sEvent f

  case lookupAction (enterActions effectRegistry) (Proxy @nextState) of
    Nothing -> do
      pure postTransitionMachine
    Just onEnter -> do
      context' <- onEnter event $ getContext postTransitionMachine
      pure $ setContext postTransitionMachine context'

terminateE ::
     ( Monad m
     , Typeable currentState
     , SingI currentState
     , ValidState Terminal currentState (SchemaEndStates schema)
     , SchemaStateType schema ~ KindOf currentState
     )
  => Machine schema currentState context 
  -> EffectRegistry schema m context
  -> m (Sing currentState, context currentState)
terminateE machine effectRegistry = do
  case lookupAction (exitActions effectRegistry) Proxy of
    Nothing -> do
      pure $ terminate machine
    Just onExit -> do
      let (st, context) = terminate machine
      context' <- onExit Terminate context
      pure (st, context')
