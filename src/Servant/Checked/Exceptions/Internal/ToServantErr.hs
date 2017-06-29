{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines a typeclass of error types that can be serialized to
-- 'ServantErr'.
module Servant.Checked.Exceptions.Internal.ToServantErr
  ( ToServantErr
  , toServantErr
  , toServantErrJson
  ) where

import Data.Functor.Identity (Identity, runIdentity)
import Data.Aeson (ToJSON, encode)
import Servant.Server (ServantErr(..))
import Servant.Checked.Exceptions.Internal.Union (Union, union, absurdUnion)

class ToServantErr a where
  toServantErr :: a -> ServantErr

instance ToServantErr a => ToServantErr (Identity a) where
  toServantErr = toServantErr . runIdentity

instance ToServantErr (Union f '[]) where
  toServantErr = absurdUnion

instance (ToServantErr (f a), ToServantErr (Union f as))
    => ToServantErr (Union f (a ': as)) where
  toServantErr = union toServantErr toServantErr

-- | @toServantErrJson code reason'
--
-- An implementation of 'toServantErr' that serializes the value as JSON and
-- returns the provided status code and reason phrase.
toServantErrJson :: ToJSON a => Int -> String -> a -> ServantErr
toServantErrJson code reason value =
  ServantErr
    { errHTTPCode = code
    , errReasonPhrase = reason
    , errBody = encode value
    , errHeaders = [("Content-Type", "application/json")]
    }
