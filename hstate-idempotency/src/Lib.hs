{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Lib where

import Control.Monad.Indexed
import Control.Monad.Indexed.State
import Control.Monad.Reader
import Data.ByteString (ByteString)
import HState.Core
import HState.Effects

newtype IdempotencyKey = IdempotencyKey ByteString

data PhaseResult resp
  = RecoveryPoint 
  | Response resp
  | NoOp

data IdempotencyDetails initialRequest = IdempotencyDetails
  { idempotencyKey :: Maybe IdempotencyKey
  , initialRequest :: initialRequest
  }

data IdempotencyState schema context m initialRequest = IdempotencyState 
  { registry :: EffectRegistry schema m context
  , idempotencyDetails :: IdempotencyDetails initialRequest
  }

newtype IdempotencyT schema context initialRequest m initialState currentState a = IdempotencyT 
  { unIdempotencyT :: IxStateT 
      (ReaderT (IdempotencyState schema context m initialRequest) m)
      (Machine schema initialState context)
      (Machine schema currentState context)
      a
  }

instance (Monad m) => IxFunctor (IdempotencyT schema context initialRequest m) where
  imap f (IdempotencyT m) = IdempotencyT $ imap f m

instance (Monad m) => IxPointed (IdempotencyT schema context initialRequest m) where
  ireturn = IdempotencyT . ireturn

instance (Monad m) => IxApplicative (IdempotencyT schema context initialRequest m) where
  iap (IdempotencyT m) (IdempotencyT m') = IdempotencyT $ iap m m'

instance (Monad m) => IxMonad (IdempotencyT schema context initialRequest m) where
  ibind f (IdempotencyT m) = IdempotencyT $ ibind (unIdempotencyT . f) m

runIdempotencyT :: 
  (Monad m) => 
  EffectRegistry schema m context ->
  IdempotencyDetails initialRequest ->
  Machine schema initialState context ->
  IdempotencyT schema context initialRequest m initialState currentState a -> 
  m (a, Machine schema currentState context)
runIdempotencyT registry initialization machine (IdempotencyT m) = 
  runReaderT (runIxStateT m $ machine) $ IdempotencyState registry initialization

idempotently :: 
  (Monad m, ValidTerminalEvent schema currentState) =>
  EffectRegistry schema m context ->
  IdempotencyDetails initialRequest ->
  Machine schema initialState context ->
  IdempotencyT schema context initialRequest m initialState currentState response ->
  m (response, Machine schema currentState context)
idempotently registry initialization machine m = 
  runIdempotencyT registry initialization machine m

noOp :: Sing currentState -> m () -> IdempotencyT schema context initialRequest m initialState currentState ()
noOp = _

response :: Sing currentState -> response -> IdempotencyT schema context initialRequest m initialState currentState ()
response 

{-
CREATE TABLE idempotency_keys (
id              BIGSERIAL   PRIMARY KEY,
created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
idempotency_key TEXT        NOT NULL CHECK (char_length(idempotency_key) <= 100),
last_run_at     TIMESTAMPTZ NOT NULL DEFAULT NOW(),
locked_at       TIMESTAMPTZ DEFAULT now(),

-- parameters of the incoming request
request_method  TEXT        NOT NULL CHECK (char_length(request_method) <= 10),
request_params  JSONB       NOT NULL,
request_path    TEXT        NOT NULL CHECK (char_length(request_path) <= 100),

-- for finished requests, stored status code and body
response_code   INT         NULL,
response_body   JSONB       NULL,

recovery_point  TEXT        NOT NULL CHECK (char_length(recovery_point) <= 50),
user_id         BIGINT      NOT NULL
);
-}