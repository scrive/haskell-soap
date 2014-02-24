{-# LANGUAGE OverloadedStrings #-}

module Network.SOAP.Transport.HTTP
    (
      -- * Initialization
      initTransport, initTransport_, initTransportWith
    , confTransport, confTransportWith
    , EndpointURL
      -- * Making a request
    , RequestP, traceRequest
      -- * Processing a response
    , BodyP, iconv, traceBody
      -- * Raw transport function
    , runQuery
    ) where

import Text.XML
import Network.HTTP.Client
-- import Control.Monad.Trans.Resource

import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Config)
import           Codec.Text.IConv (EncodingName, convertFuzzy, Fuzzy(Transliterate))
-- import qualified Network.TLS.Extra as TLS

import           Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy.Char8 (ByteString, unpack)

import Debug.Trace (trace)
--import Control.Exception as E
import Data.Monoid ((<>))

import Network.SOAP.Transport

-- | Update request record after defaults and method-specific fields are set.
type RequestP = Request -> Request

-- | Process response body to make it a nice UTF8-encoded XML document.
type BodyP = ByteString -> ByteString

-- | Web service URL. Configured at initialization, but you can tweak it
--   dynamically with a request processor.
type EndpointURL = String


-- | Create a http-client transport. Use identity transformers if you
--   don't need any special treatment.
initTransport :: EndpointURL
              -> RequestP
              -> BodyP
              -> IO Transport
initTransport = initTransportWith defaultManagerSettings

-- | Create a transport without any request and body processing.
initTransport_ :: EndpointURL -> IO Transport
initTransport_ url = initTransport url id id

-- | Create a http-client transport using manager settings (for plugging tls etc.).
initTransportWith :: ManagerSettings
                  -> EndpointURL
                  -> RequestP
                  -> BodyP
                  -> IO Transport
initTransportWith settings url updateReq updateBody = do
    manager <- newManager settings
    return $! runQuery manager url updateReq updateBody

-- | Load common transport parameters from a configurator file.
--
-- > soap {
-- >   url = "https://vendor.tld/service/"
-- >   trace = true
-- >   timeout = 15
-- > }
--
-- Only url field is required.
--
-- > import Data.Configurator (load, Worth(Required))
-- > main = do
-- >     transport <- confTransport "soap" =<< load [Required "etc/example.conf"]

confTransport :: Text -> Config -> IO Transport
confTransport section conf = confTransportWith defaultManagerSettings section conf id id

-- | A more extensible transport parameter loader.
confTransportWith :: ManagerSettings
                  -> Text
                  -> Config
                  -> RequestP
                  -> BodyP
                  -> IO Transport
confTransportWith settings section conf brp bbp = do
    url <- Conf.require conf (section <> ".url")

    tracer <- Conf.lookupDefault False conf (section <> ".trace")
    let (tr, tb) = if tracer
                       then (traceRequest, traceBody)
                       else (id, id)

    timeout <- Conf.lookupDefault 15 conf (section <> ".timeout")
    let to r = r { responseTimeout = Just (timeout * 1000000) }

    encoding <- Conf.lookup conf (section <> ".encoding")
    let ic = maybe id iconv encoding

    initTransportWith settings url (to . tr . brp) (tb . ic . bbp)

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
    res <- httpLbs (updateReq request') manager
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
