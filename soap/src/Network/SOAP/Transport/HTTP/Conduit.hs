{-# LANGUAGE OverloadedStrings #-}

-- | A feature-rich http-conduit based transport allowing to deal with
--   HTTPS, authentication and other stuff using request and body processors.

module Network.SOAP.Transport.HTTP.Conduit
    (
      -- * Initialization
      initTransport, initTransport_
    , EndpointURL
      -- * Making a request
    , RequestP, clientCert, traceRequest
      -- * Processing a response
    , BodyP, iconv, traceBody
      -- * Raw transport function
    , runQuery
    ) where

import Text.XML
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource
import           Codec.Text.IConv (EncodingName, convertFuzzy, Fuzzy(Transliterate))
import qualified Network.TLS.Extra as TLS

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Debug.Trace (trace)

import Network.SOAP.Transport

-- | Update request record after defaults and method-specific fields are set.
type RequestP = Request (ResourceT IO) -> Request (ResourceT IO)

-- | Process response body to make it a nice UTF8-encoded XML document.
type BodyP = ByteString -> ByteString

-- | Web service URL. Configured at initialization, but you can tweak it
--   dynamically with a request processor.
type EndpointURL = String

-- | Create a http-conduit transport. Use identity transformers if you
--   don't need any special treatment.
initTransport :: EndpointURL
              -> RequestP
              -> BodyP
              -> IO Transport
initTransport url updateReq updateBody = do
    manager <- newManager def
    return $! runQuery manager url updateReq updateBody

-- | Create a transport without any request and body processing.
initTransport_ :: EndpointURL -> IO Transport
initTransport_ url = initTransport url id id

-- | Render document, submit it as a POST request and retrieve a body.
runQuery :: Manager
         -> EndpointURL
         -> RequestP
         -> BodyP
         -> Transport
runQuery manager url updateReq updateBody soapAction doc = do
    let body = renderLBS def $! doc

    request <- parseUrl url
    let request' = request { method          = "POST"
                           , responseTimeout = Just 15000000
                           , requestBody     = RequestBodyLBS body
                           , requestHeaders  = [ ("Content-Type", "text/xml; charset=utf-8")
                                               , ("SOAPAction", BS.pack soapAction)
                                               ]
                           }
    res <- runResourceT $ httpLbs (updateReq request') manager
    return . updateBody . responseBody $ res

-- * Some common processors.

-- | Create an IConv-based processor.
iconv :: EncodingName -> BodyP
iconv src = convertFuzzy Transliterate src "UTF-8"

-- | Show a debug dump of a response body.
traceBody :: BodyP
traceBody lbs = trace "response:" $ trace (unpack lbs) lbs

-- | Show a debug dump of a request body.
traceRequest :: RequestP
traceRequest r = trace "request:" $ trace (showBody $ requestBody r) r
    where
        showBody (RequestBodyLBS body) = unpack body
        showBody _ = "<dynamic body>"

-- | Load certificate, key and make a request processor setting them.
clientCert :: FilePath -- ^ Path to a certificate.
           -> FilePath -- ^ Path to a private key.
           -> IO RequestP
clientCert certPath keyPath = do
    cert <- TLS.fileReadCertificate certPath
    pkey <- TLS.fileReadPrivateKey keyPath

    return $ \req -> req { clientCertificates = [(cert, Just pkey)] }
