-- | This package comes with a single transport, but the your vendor's
-- SOAP implementation can behave very differently, so invokeWS can be
-- rigged to use anything that follows a simple interface.

module Network.SOAP.Transport
    (
      Transport
    ) where

import Text.XML (Document)
import Data.ByteString.Lazy.Char8 (ByteString)

-- | Common transport type. Get a request and deliver it to an endpoint
--   specified during initialization.
type Transport = String   -- ^ SOAPAction header
              -> Document -- ^ XML document with a SOAP request
              -> IO ByteString
