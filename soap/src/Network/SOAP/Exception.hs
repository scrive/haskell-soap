{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Network.SOAP.Exception
    ( SOAPParsingError(..)
    , SOAPFault(..), extractSoapFault
    ) where

import Control.Exception as E
import Data.Typeable
import Text.XML (Document)
import Text.XML.Cursor
import qualified Data.Text as T

data SOAPParsingError = SOAPParsingError String deriving (Show, Typeable)
instance Exception SOAPParsingError

-- | Exception to be thrown when transport encounters an exception that is
--   acutally a SOAP Fault.
data SOAPFault = SOAPFault { faultCode   :: T.Text
                           , faultString :: T.Text
                           , faultDetail :: T.Text
                           } deriving (Eq, Show, Typeable)

instance Exception SOAPFault

-- | Try to find a SOAP Fault in a document.
extractSoapFault :: Document -> Maybe SOAPFault
extractSoapFault doc =
    case cur' of
        []    -> Nothing
        cur:_ -> Just $ SOAPFault { faultCode   = peek "faultcode" cur
                                  , faultString = peek "faultstring" cur
                                  , faultDetail = peek "detail" cur
                                  }
    where
        cur' = fromDocument doc $| laxElement "Envelope"
                                &/ laxElement "Body"
                                &/ laxElement "Fault"

        peek name cur = T.concat $ cur $/ laxElement name &/ content
