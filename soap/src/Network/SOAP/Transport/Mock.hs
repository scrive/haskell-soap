-- | Debug transport to train your parsers without bugging real services.

module Network.SOAP.Transport.Mock
    (
      initTransport
    , Handler, Handlers
    , runQuery
    ) where

import Network.SOAP.Transport

import Text.XML
import Data.ByteString.Lazy.Char8 as LBS

type Handler = Document -> IO LBS.ByteString
type Handlers = [(String, Handler)]

initTransport :: Handlers -> IO Transport
initTransport handlers = return $ runQuery handlers

runQuery :: [(String, Handler)] -> Transport
runQuery handlers soapAction doc = do
    case lookup soapAction handlers of
        Nothing -> error $ "No handler for action " ++ soapAction
        Just handler -> handler doc
