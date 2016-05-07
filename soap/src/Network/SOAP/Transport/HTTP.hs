{-# LANGUAGE OverloadedStrings #-}

module Network.SOAP.Transport.HTTP
    (
      -- * Initialization
      initTransportWithM
    , EndpointURL
      -- * Making a request
    , RequestProc, printRequest
      -- * Processing a response
    , BodyProc, printBody
      -- * Raw transport function
    , runQueryM
      -- * Deprecated
    , initTransport, initTransport_, initTransportWith
    , confTransport, confTransportWith
    , RequestP, traceRequest
    , BodyP, iconv, traceBody
    , runQuery
    ) where

import Text.XML
import Network.HTTP.Client

import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Config)
import           Codec.Text.IConv (EncodingName, convertFuzzy, Fuzzy(Transliterate))

import           Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.ByteString.Lazy.Char8 (ByteString, unpack)

import Debug.Trace (trace)
import Data.Monoid ((<>))

import Network.SOAP.Transport

-- | Update request record after defaults and method-specific fields are set.
type RequestProc = Request -> IO Request

type RequestP = Request -> Request

-- | Process response body to make it a nice UTF8-encoded XML document.
type BodyProc = ByteString -> IO ByteString

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

initTransportWith :: ManagerSettings
                  -> EndpointURL
                  -> RequestP
                  -> BodyP
                  -> IO Transport
initTransportWith settings url updateReq updateBody = do
    manager <- newManager settings
    return $! runQuery manager url updateReq updateBody

-- | Create a http-client transport using manager settings (for plugging tls etc.).
initTransportWithM :: ManagerSettings
                   -> EndpointURL
                   -> RequestProc
                   -> BodyProc
                   -> IO Transport
initTransportWithM settings url requestProc bodyProc = do
    manager <- newManager settings
    return $! runQueryM manager url requestProc bodyProc

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

runQuery :: Manager
         -> EndpointURL
         -> RequestP
         -> BodyP
         -> Transport
runQuery manager url updateReq updateBody =
    runQueryM manager url (pure . updateReq) (pure . updateBody)

-- | Render document, submit it as a POST request and retrieve a body.
runQueryM :: Manager
          -> EndpointURL
          -> RequestProc
          -> BodyProc
          -> Transport
runQueryM manager url requestProc bodyProc soapAction doc = do
    let body = renderLBS def $! doc

    request <- parseUrl url
    request' <- requestProc request
        { method          = "POST"
        , responseTimeout = Just 15000000
        , requestBody     = RequestBodyLBS body
        , requestHeaders  = [ ("Content-Type", "text/xml; charset=utf-8")
                            , ("SOAPAction", BS.pack soapAction)
                            ]
        , checkStatus = \_ _ _ -> Nothing
        }

    httpLbs request' manager >>= bodyProc . responseBody

-- * Some common processors.

-- | Create an IConv-based processor.
iconv :: EncodingName -> BodyP
iconv src = convertFuzzy Transliterate src "UTF-8"

-- | Show a debug dump of a response body.
traceBody :: BodyP
traceBody lbs = trace "response:" $ trace (unpack lbs) lbs

printBody :: BodyProc
printBody lbs = do
    BSL.putStrLn $ "response:" <> lbs
    pure lbs

-- | Show a debug dump of a request body.
traceRequest :: RequestP
traceRequest r = trace "request:" $ trace (showBody $ requestBody r) r
    where
        showBody (RequestBodyLBS body) = unpack body
        showBody _ = "<dynamic body>"

printRequest :: RequestProc
printRequest req = do
    BSL.putStrLn $ "request:" <> bslBody (requestBody req)
    pure req
    where
        bslBody (RequestBodyLBS body) = body
        bslBody _ = "<dynamic body>"

{-# DEPRECATED initTransportWith, RequestP, traceRequest, BodyP, traceBody, runQuery "Processors were lifted to IO." #-}

