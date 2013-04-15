{-# LANGUAGE OverloadedStrings #-}
-- | Debug transport to train your parsers without bugging real services.

module Network.SOAP.Transport.Mock
    (
      initTransport
    , Handler, Handlers
    , handler
    , runQuery
    ) where

import Network.SOAP.Transport

import Text.XML
import Text.XML.Writer
import Data.ByteString.Lazy.Char8 as LBS

type Handler = Document -> IO LBS.ByteString
type Handlers = [(String, Handler)]

-- | Wrap a collection of handlers into a transport.
initTransport :: Handlers -> IO Transport
initTransport handlers = return $ runQuery handlers

-- | Choose and apply a handler.
runQuery :: [(String, Handler)] -> Transport
runQuery handlers soapAction doc = do
    case lookup soapAction handlers of
        Nothing -> error $ "No handler for action " ++ soapAction
        Just handler -> handler doc

-- | Process a Document and wrap result in a SOAP Envelope.
handler :: (ToXML a) => (Document -> IO a) -> Handler
handler h doc = do
    result <- h doc
    return . renderLBS def
           . document (sname "Envelope")
           . element (sname "Body")
           . toXML
           $ result
    where
        sname n = Name n (Just "http://schemas.xmlsoap.org/soap/envelope/") (Just "soapenv")
