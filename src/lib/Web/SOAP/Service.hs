{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Web.SOAP.Service
    ( invokeWS
    , SOAPSettings(..)
    ) where

import           Text.XML
import           Text.XML.Cursor
import           Network.HTTP.Conduit
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Encoding as TE
import qualified Codec.Text.IConv as IC
import           Data.Monoid ((<>))

import Web.SOAP.Schema.Types

data SOAPSettings = SOAPSettings {
    soapURL :: String,
    soapNamespace :: Text,
    soapCodepage :: IC.EncodingName
} deriving (Read, Show)


invokeWS :: (ToElements h, ToElements i, FromCursor o)
         => SOAPSettings  -- web service configuration
         -> Text          -- SOAPAction header
         -> h             -- request headers
         -> i             -- request body
         -> IO o          -- response

invokeWS SOAPSettings{..} methodHeader h b = do
    let doc = document $! envelope (toElements h) (toElements b)
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

envelope :: [Element] -> [Element] -> Element
envelope h b =
    Element
        "{http://schemas.xmlsoap.org/soap/envelope/}Envelope"
        def
        [ NodeElement $! Element "{http://schemas.xmlsoap.org/soap/envelope/}Header" def (map NodeElement h)
        , NodeElement $! Element "{http://schemas.xmlsoap.org/soap/envelope/}Body" def (map NodeElement b)
        ]
