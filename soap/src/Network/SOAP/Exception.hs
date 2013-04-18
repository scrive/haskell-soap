{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Network.SOAP.Exception
    ( SOAPFault(..)
    , extractSoapFault
    ) where

import Control.Exception as E
import Data.Typeable
import Text.XML (Document)
import Text.XML.Cursor
import qualified Data.Text as T

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
        cur:_ -> Just $ SOAPFault { faultCode   = code cur
                                  , faultString = string cur
                                  , faultDetail = detail cur
                                  }
    where
        cur' = fromDocument doc $| laxElement "Envelope"
                                &/ laxElement "Body"
                                &/ laxElement "Fault"

        code   cur = T.concat $ cur $/ laxElement "faultcode"   &/ content
        string cur = T.concat $ cur $/ laxElement "faultstring" &/ content
        detail cur = T.concat $ cur $/ laxElement "detail"      &/ content
