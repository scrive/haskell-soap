{-# LANGUAGE OverloadedStrings #-}
-- | Debug transport to train your parsers without bugging real services.

module Network.SOAP.Transport.Mock
    (
      initTransport
    , Handler, Handlers
    , handler, fault
    , runQuery
    ) where

import Network.SOAP.Transport

import Text.XML
import Text.XML.Writer
import Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)

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
        Just h -> h doc

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

-- | Emulate a SOAP fault.
fault :: Text -- ^ SOAP Fault code (e.g. «soap:Server»)
      -> Text -- ^ Fault string
      -> Text -- ^ Fault detail
      -> Handler
fault c s d = handler . const . return $
    element "Fault" $ do
        element "faultcode" c
        element "faultstring" s
        element "detail" d
