-- | A heart of the package, 'invokeWS' assembles and executes requests.

{-# LANGUAGE OverloadedStrings, Rank2Types, FlexibleContexts #-}
module Network.SOAP
    (
      invokeWS, Transport
    , ResponseParser(..)
    , Parser
    ) where

import Network.SOAP.Transport (Transport)

import Data.Conduit

import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.Default (def)
import qualified Text.XML as XML
import           Text.XML.Cursor as XML
import qualified Text.XML.Stream.Parse as XSP
import           Data.XML.Types (Event)
import           Text.XML.Writer (ToXML, soap)

-- | Different parsing modes available to extract reply contents.
data ResponseParser a = StreamParser (Parser a)            -- ^ Streaming parser from Text.XML.Stream.Parse
                      | CursorParser (XML.Cursor -> a)     -- ^ XPath-like parser from Text.XML.Cursor
                      | DocumentParser (XML.Document -> a) -- ^ Parse raw XML document.
                      | RawParser (LBS.ByteString -> a)    -- ^ Work with a raw bytestring.

-- | Stream parser from Text.XML.Stream.Parse.
type Parser a = Sink Event (ResourceT IO) a

-- | Prepare data, assemble request and apply a parser to a response.
invokeWS :: (ToXML h, ToXML b)
         => Transport        -- ^ Configured transport to make requests with.
         -> String           -- ^ SOAPAction header.
         -> h                -- ^ SOAP Header element. () or Nothing will result in omiting the Header node. Put a comment if you need an empty element present.
         -> b                -- ^ SOAP Body element.
         -> ResponseParser a -- ^ Parser to use on a request reply.
         -> IO a
invokeWS transport soapAction header body parser = do
    lbs <- transport soapAction $! soap header body
    case parser of
        StreamParser sink -> runResourceT $ XSP.parseLBS def lbs $$ unwrapEnvelopeSink sink
        CursorParser func -> return . func . unwrapEnvelopeCursor . XML.fromDocument $ XML.parseLBS_ def lbs
        DocumentParser func -> return . func $ XML.parseLBS_ def lbs
        RawParser func    -> return . func $ lbs

unwrapEnvelopeSink :: Parser a -> Parser a
unwrapEnvelopeSink sink = XSP.force "No SOAP Envelope" $ XSP.tagNoAttr "{http://schemas.xmlsoap.org/soap/envelope/}Envelope"
                        $ XSP.force "No SOAP Body" $ XSP.tagNoAttr "{http://schemas.xmlsoap.org/soap/envelope/}Body"
                        $ sink

unwrapEnvelopeCursor :: Cursor -> Cursor
unwrapEnvelopeCursor c = forceCur $ c $| laxElement "Envelope" &/ laxElement "Body"
    where forceCur [] = error "No SOAP Body"
          forceCur (x:_) = x
