{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Servant.Checked.Exceptions.Internal.Servant.Server

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module exports 'HasServer' instances for 'Throws' and 'Throwing'.
-}

module Servant.Checked.Exceptions.Internal.Servant.Server () where

import Data.Proxy (Proxy(Proxy))
import Control.Monad.Error.Class (throwError)
import Servant.Server.Internal.Router (Router)
import Servant.Server.Internal.RoutingApplication (Delayed)
import Servant
       (Context, Handler, HasServer(..), ServerT, Verb, (:>), (:<|>))

import Servant.Checked.Exceptions.Internal.Envelope (Envelope(..))
import Servant.Checked.Exceptions.Internal.Servant.API
       (Throws, Throwing, ThrowingNonterminal)
import Servant.Checked.Exceptions.Internal.Union (OpenUnion)
import Servant.Checked.Exceptions.Internal.ToServantErr (ToServantErr, toServantErr)

-- TODO: Make sure to also account for when headers are being used.

-- | Change a 'Throws' into 'Throwing'.
instance (HasServer (Throwing '[e] :> api) context) =>
    HasServer (Throws e :> api) context where

  type ServerT (Throws e :> api) m =
    ServerT (Throwing '[e] :> api) m

  route
    :: Proxy (Throws e :> api)
    -> Context context
    -> Delayed env (ServerT (Throwing '[e] :> api) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (Throwing '[e] :> api))

-- | When @'Throwing' es@ comes before a 'Verb', change it into the same 'Verb'
-- but returning an @'Envelope' es@.
instance
      ( ToServantErr (OpenUnion es)
      , HasServer (Verb method status ctypes (a :: *)) context ) =>
    HasServer (Throwing es :> Verb method status ctypes a) context where

  type ServerT (Throwing es :> Verb method status ctypes a) m =
    ServerT (Verb method status ctypes (Envelope es a)) m

  route
    :: forall env.
       Proxy (Throwing es :> Verb method status ctypes a)
    -> Context context
    -> Delayed env (ServerT (Verb method status ctypes (Envelope es a)) Handler)
    -> Router env
  route _ context subserver =
    route (Proxy :: Proxy (Verb method status ctypes a)) context
      (fmap (>>= handleEnvelope) subserver)

handleEnvelope :: ToServantErr (OpenUnion es) => Envelope es a -> Handler a
handleEnvelope (SuccEnvelope result) = pure result
handleEnvelope (ErrEnvelope err) = throwError (toServantErr err)

-- | When @'Throwing' es@ comes before ':<|>', push @'Throwing' es@ into each
-- branch of the API.
instance HasServer ((Throwing es :> api1) :<|> (Throwing es :> api2)) context =>
    HasServer (Throwing es :> (api1 :<|> api2)) context where

  type ServerT (Throwing es :> (api1 :<|> api2)) m =
    ServerT ((Throwing es :> api1) :<|> (Throwing es :> api2)) m

  route
    :: Proxy (Throwing es :> (api1 :<|> api2))
    -> Context context
    -> Delayed env (ServerT ((Throwing es :> api1) :<|> (Throwing es :> api2)) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy ((Throwing es :> api1) :<|> (Throwing es :> api2)))

-- | When a @'Throws' e@ comes immediately after a @'Throwing' es@, 'Snoc' the
-- @e@ onto the @es@. Otherwise, if @'Throws' e@ comes before any other
-- combinator, push it down so it is closer to the 'Verb'.
instance HasServer (ThrowingNonterminal (Throwing es :> api :> apis)) context =>
    HasServer (Throwing es :> api :> apis) context where

  type ServerT (Throwing es :> api :> apis) m =
    ServerT (ThrowingNonterminal (Throwing es :> api :> apis)) m

  route
    :: Proxy (Throwing es :> api :> apis)
    -> Context context
    -> Delayed env (ServerT (ThrowingNonterminal (Throwing es :> api :> apis)) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (ThrowingNonterminal (Throwing es :> api :> apis)))
