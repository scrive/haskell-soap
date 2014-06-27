{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Text.Read
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.SOAP
import           Network.SOAP.Transport.HTTP
import           Text.XML.Writer
import qualified Text.XML.Stream.Parse as P

data Response = Response {
  responseSuccess :: Bool
, responseText :: Text
, responseState :: Text
, responseCity :: Text
, responseWeatherStation :: Text
, responseWeatherId :: Int
, responseDescription :: Text
, responseTemperature :: Int
, responseRelativeHumidity :: Int
, responseWind :: Text
, responsePressure :: Text
, responseVisibility :: Text
, responseWindChill :: Text
, responseRemarks :: Text
} deriving (Eq, Show)

main :: IO ()
main = do
  transport <- initTransport "http://wsf.cdyne.com/WeatherWS/Weather.asmx" id id
  invokeWS transport "http://ws.cdyne.com/WeatherWS/GetCityWeatherByZIP" () requestBody (StreamParser responseParser) >>= print

requestBody :: XML
requestBody =
    elementA "GetCityWeatherByZIP" [("xmlns", "http://ws.cdyne.com/WeatherWS/")]
  $ element "ZIP" ("10007" :: Text)

responseParser :: Parser Response
responseParser = tag "GetCityWeatherByZIPResponse" $ tag "GetCityWeatherByZIPResult" response
  where
    response = Response
      <$> tagContentAsBool "Success"
      <*> tagContent "ResponseText"
      <*> tagContent "State"
      <*> tagContent "City"
      <*> tagContent "WeatherStationCity"
      <*> tagContentAsInt "WeatherID"
      <*> tagContent "Description"
      <*> tagContentAsInt "Temperature"
      <*> tagContentAsInt "RelativeHumidity"
      <*> tagContent "Wind"
      <*> tagContent "Pressure"
      <*> tagContent "Visibility"
      <*> tagContent "WindChill"
      <*> tagContent "Remarks"
    tag name = P.force ("no " ++ name) . P.tagNoAttr (fromString $ "{http://ws.cdyne.com/WeatherWS/}" ++ name)
    tagContent name = tag name P.content
    tagContentAsInt name = (P.force ("invalid " ++ name) $ readMaybe . T.unpack <$> tagContent name)
    tagContentAsBool = fmap (== "true") . tagContent
