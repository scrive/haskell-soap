{-# LANGUAGE OverloadedStrings #-}

-- | A feature-rich http-conduit based transport allowing to deal with
--   HTTPS, authentication and other stuff using request and body processors.

module Network.SOAP.Transport.HTTP.Conduit
    (
      -- * Initialization
      initTransport, initTransport_, confTransport
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

import           Data.Configurator (require, lookupDefault)
import           Data.Configurator.Types (Config)
import           Codec.Text.IConv (EncodingName, convertFuzzy, Fuzzy(Transliterate))
import qualified Network.TLS.Extra as TLS

import           Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy.Char8 (ByteString, unpack)

import Debug.Trace (trace)
--import Control.Exception as E
import Data.Monoid ((<>))

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

-- | Load common transport parameters from a configurator file.
--
-- > soap {
-- >   url = "https://vendor.tld/service/"
-- >   client_cert = "etc/client.pem"
-- >   client_key = "etc/client.key"
-- >   trace = true
-- > }
--
-- Only url field is required.
--
-- > import Data.Configurator (load, Worth(Required))
-- > main = do
-- >     transport <- confTransport "soap" =<< load [Required "etc/example.conf"]
confTransport :: Text -> Config -> IO Transport
confTransport section conf = do
    url <- require conf (section <> ".url")

    cCert <- lookupDefault "" conf (section <> ".client_cert")
    cKey <- lookupDefault "" conf (section <> ".client_key")
    cc <- if null cCert
              then return id
              else clientCert cCert cKey

    tracer <- lookupDefault False conf (section <> ".trace")
    let (tr, tb) = if tracer
                       then (traceRequest, traceBody)
                       else (id, id)

    initTransport url (tr . cc) tb

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
                           , checkStatus = \_ _ _ -> Nothing
                           }
    res <- (runResourceT $ httpLbs (updateReq request') manager)
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
