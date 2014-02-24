-- | SSL-enabled http transport with support for https requests and client certificates.

module Network.SOAP.Transport.HTTP.OpenSSL
    ( confTransport
    , makeSettings
    -- * OpenSSL parts
    , withOpenSSL, SSLContext
    ) where

import Network.HTTP.Client (ManagerSettings)
import Network.SOAP.Transport (Transport)
import Network.SOAP.Transport.HTTP (confTransportWith)

import Network.HTTP.Client.OpenSSL
import OpenSSL.Session as SSL

import           Data.Text (Text)
import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Config)
import           Control.Monad (liftM2)

-- | Initialize a SOAP HTTP transport with HTTPS support using tls.
confTransport :: Text   -- ^ Section name containing transport settings.
              -> Config
              -> (SSLContext -> IO ())
              -> IO Transport
confTransport section conf updCtx = do
    let get = Conf.lookup (Conf.subconfig section conf)
    cert <- get "client_cert"
    key <-  get "client_key"
    settings <- makeSettings (liftM2 (,) cert key) updCtx
    confTransportWith settings section conf id id

makeSettings :: Maybe (FilePath, FilePath) -- ^ Client certificate and key.
             -> (SSLContext -> IO ())      -- ^ Additional OpenSSL setup if needed.
             -> IO ManagerSettings
makeSettings clientCert updateContext = return . opensslManagerSettings $ do
    ctx <- SSL.context
    case clientCert of
        Nothing -> return ()
        Just (certFile, keyFile) -> do
            SSL.contextSetCertificateFile ctx certFile
            SSL.contextSetPrivateKeyFile ctx keyFile
    updateContext ctx
    return ctx
