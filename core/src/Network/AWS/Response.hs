{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.Response
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Response where

import Control.Applicative    (pure)
import Control.Exception      (throwIO)
import Control.Monad          (void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text   (Text)

import Network.AWS.Data.ByteString
import Network.AWS.Data.Log
import Network.AWS.Data.XML
import Network.AWS.Types
import Network.HTTP.Client         ()

import Text.XML (Node)

import Network.HTTP.Client (BodyReader)
import Network.HTTP.Types  (ResponseHeaders)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client  as Client

receiveNull :: Rs a BodyReader
            -> Logger
            -> Service
            -> Proxy a
            -> ClientResponse
            -> IO (Response a)
receiveNull rs _ = stream (\_ _ x -> sinkNull x >> pure (Right rs))

receiveEmpty :: ( Int
               -> ResponseHeaders
               -> ()
               -> Either String (Rs a BodyReader)
                )
             -> Logger
             -> Service
             -> Proxy a
             -> ClientResponse
             -> IO (Response a)
receiveEmpty f _ = stream (\s h x -> sinkNull x >> pure (f s h ()))

receiveXMLWrapper :: Text
                  -> ( Int
                    -> ResponseHeaders
                    -> [Node]
                    -> Either String (Rs a BodyReader)
                     )
                  -> Logger
                  -> Service
                  -> Proxy a
                  -> ClientResponse
                  -> IO (Response a)
receiveXMLWrapper n f = receiveXML (\s h x -> x .@ n >>= f s h)

receiveXML :: ( Int
             -> ResponseHeaders
             -> [Node]
             -> Either String (Rs a BodyReader)
              )
           -> Logger
           -> Service
           -> Proxy a
           -> ClientResponse
           -> IO (Response a)
receiveXML = deserialise decodeXML

receiveJSON :: ( Int
              -> ResponseHeaders
              -> Object
              -> Either String (Rs a BodyReader)
               )
            -> Logger
            -> Service
            -> Proxy a
            -> ClientResponse
            -> IO (Response a)
receiveJSON = deserialise eitherDecode'

receiveBody :: ( Int
              -> ResponseHeaders
              -> BodyReader
              -> Either String (Rs a BodyReader)
               )
            -> Logger
            -> Service
            -> Proxy a
            -> ClientResponse
            -> IO (Response a)
receiveBody f _ = stream (\s h x -> pure (f s h x))

-- | Deserialise an entire response body, such as an XML or JSON payload.
deserialise :: (LazyByteString -> Either String b)
            -> ( Int
              -> ResponseHeaders
              -> b
              -> Either String (Rs a BodyReader)
               )
            -> Logger
            -> Service
            -> Proxy a
            -> ClientResponse
            -> IO (Response a)
deserialise g f l Service{..} _ rs = do
    let s = Client.responseStatus  rs
        h = Client.responseHeaders rs
        x = Client.responseBody    rs
    b <- sinkLBS x
    if not (_svcCheck s)
        then throwIO (_svcError s h b)
        else do
            liftIO . l Debug . build $ "[Raw Response Body] {\n" <> b <> "\n}"
            case g b >>= f (fromEnum s) h of
                Right r -> pure (s, r)
                Left  e -> throwIO . SerializeError $
                    SerializeError' _svcAbbrev s (Just b) e

-- | Stream a raw response body, such as an S3 object payload.
stream :: ( Int
         -> ResponseHeaders
         -> BodyReader
         -> IO (Either String (Rs a BodyReader))
          )
       -> Service
       -> Proxy a
       -> ClientResponse
       -> IO (Response a)
stream f Service{..} _ rs = do
    let s = Client.responseStatus  rs
        h = Client.responseHeaders rs
        x = Client.responseBody    rs
    if not (_svcCheck s)
        then sinkLBS x >>= throwIO . _svcError s h
        else do
            res <- f (fromEnum s) h x
            case res of
                Right r -> pure (s, r)
                Left  e -> throwIO . SerializeError $
                    SerializeError' _svcAbbrev s Nothing e

sinkLBS :: BodyReader -> IO LazyByteString
sinkLBS = fmap LBS.fromChunks . Client.brConsume

sinkNull :: BodyReader -> IO ()
sinkNull = void . Client.brConsume
