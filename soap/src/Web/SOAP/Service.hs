{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Web.SOAP.Service
    ( SOAPSettings(..)
    , invokeWS
    ) where

import           Text.XML
import           Text.XML.Cursor
import           Network.HTTP.Conduit
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Codec.Text.IConv as IC
import           Data.Monoid ((<>))

import Web.SOAP.Types

-- | SOAP service parameters
data SOAPSettings = SOAPSettings {
    soapURL :: String,
    soapNamespace :: Text,
    soapCodepage :: IC.EncodingName
} deriving (Read, Show)


-- | Query a SOAP service.
invokeWS :: (ToNodes h, ToNodes i, FromCursor o)
         => SOAPSettings  -- ^ web service configuration
         -> Text          -- ^ SOAPAction header
         -> h             -- ^ request headers
         -> i             -- ^ request body
         -> IO o          -- ^ response

invokeWS SOAPSettings{..} methodHeader h b = do
    let headerNodes = toNodes h
    let bodyNodes = map (flowNS $ Just soapNamespace) (toNodes b)

    let doc = document $! envelope headerNodes bodyNodes
    let body = renderLBS def $! doc

    putStrLn "Request:"
    TL.putStrLn . renderText def { rsPretty = True } $! doc

    request <- parseUrl soapURL
    res <- withManager $ httpLbs request { method          = "POST"
                                         , responseTimeout = Just 15000000
                                         , requestBody     = RequestBodyLBS body
                                         , requestHeaders  = [ ("Content-Type", "text/xml; charset=utf-8")
                                                             , ("SOAPAction", TE.encodeUtf8 methodHeader)
                                                             ]
                                         }

    let resBody = IC.convertFuzzy IC.Transliterate soapCodepage "utf-8" $ responseBody res

    case parseLBS def resBody of
        Left err -> do
            putStrLn $ "Error: " <> show err
            putStrLn "Raw response:"
            print $ responseBody res
            error $ show err

        Right replyDoc -> do
            putStrLn "Response:"
            TL.putStrLn . renderText def { rsPretty = True } $ replyDoc
            let reply = fromDocument replyDoc
            print reply
            return $! fromCursor reply

-- ** Request components

document :: Element -> Document
document r = Document (Prologue [] Nothing []) r []

envelope :: [Node] -> [Node] -> Element
envelope h b =
    Element
        "{http://schemas.xmlsoap.org/soap/envelope/}Envelope"
        def
        [ NodeElement $! Element "{http://schemas.xmlsoap.org/soap/envelope/}Header" def h
        , NodeElement $! Element "{http://schemas.xmlsoap.org/soap/envelope/}Body" def b
        ]

-- | Little helper to apply default service namespace to body nodes and their descendants.
--   This removes the necessity to flood your code with {http://vendor.silly.web/Service.spamx} in element names.
flowNS :: Maybe Text -> Node -> Node
flowNS ns (NodeElement (Element (Name name Nothing prefix) as cs)) = NodeElement $ Element (Name name ns prefix) as $ map (flowNS ns) cs  -- update element ns and continue
flowNS ns (NodeElement (Element name@(Name _ ns' _) as cs))        = NodeElement $ Element name as                  $ map (flowNS ns') cs -- switch to new namespace and continue
flowNS ns node = node -- ignore non-elements