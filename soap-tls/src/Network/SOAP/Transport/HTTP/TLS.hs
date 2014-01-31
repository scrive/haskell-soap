-- | SSL-enabled http transport with support for https requests and client certificates.

module Network.SOAP.Transport.HTTP.TLS where

import Network.SOAP.Transport (Transport)
import Network.SOAP.Transport.HTTP (confTransportWith)

import Network.HTTP.Client.TLS
import Network.TLS
import Data.X509.Validation as X509
import Network.Connection (TLSSettings(..))

import           Data.Monoid
import           Data.Text (Text)
import           Data.Default (def)
import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Config)

confTransport :: Text -> Config -> IO Transport
confTransport section conf = do
    cert <- Conf.lookup conf (section <> ".client_cert")
    key <- Conf.lookup conf (section <> ".client_key")
    settings <- makeSettings cert key X509.validateDefault
    confTransportWith settings section conf id id

makeSettings (Just certFile) (Just keyFile) onSC = do
    creds <- either error Just `fmap` credentialLoadX509 certFile keyFile

    let onCR _ = return creds
    let hooks = def { onCertificateRequest = onCR
                    , onServerCertificate  = onSC
                    }
    let clientParams = (defaultParamsClient "" "") { clientHooks = hooks }
    return $! mkManagerSettings (TLSSettings clientParams) Nothing

makeSettings _ _ _ = return tlsManagerSettings
