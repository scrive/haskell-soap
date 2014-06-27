{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T

import           Network.SOAP (invokeWS, Transport, ResponseParser(StreamParser))
import           Network.SOAP.Transport.HTTP (initTransport)
import           Network.SOAP.Parsing.Stream (flaxTag, flaxContent, readTag)
import           Text.XML.Writer (elementA, element)

data Response = Response { rSuccess :: Bool
                         , rText :: Text
                         , rState :: Text
                         , rCity :: Text
                         , rWatherStation :: Text
                         , rWeatherId :: Int
                         , rDescription :: Text
                         , rTemperature :: Int
                         , rRelativeHumidity :: Int
                         , rWind :: Text
                         , rPressure :: Text
                         , rVisibility :: Text
                         , rWindChill :: Text
                         , rRemarks :: Text
                         } deriving (Show)

main :: IO ()
main = do
    transport <- initTransport "http://wsf.cdyne.com/WeatherWS/Weather.asmx" id id
    response <- getWeather transport "10007"
    print response

getWeather :: Transport -> Text -> IO Response
getWeather t zip = invokeWS t url () body parser
  where
    url = "http://ws.cdyne.com/WeatherWS/GetCityWeatherByZIP"
    -- To prevent xml-conduit renderer from nulling children namespaces you have to either
    -- set parent NS as an attribute (like here) or make your own element wrapper that
    -- will force namespace to its name (correct).
    -- Some servers will accept ns-nulled (<child xmlns="">) elements, but some wouldn't.
    body = elementA "GetCityWeatherByZIP" [("xmlns", "http://ws.cdyne.com/WeatherWS/")]
         $ element "ZIP" zip

    parser = StreamParser
           . flaxTag "GetCityWeatherByZIPResponse"
           . flaxTag "GetCityWeatherByZIPResult"
           $ response

    response = Response
            <$> fmap (== "true") (flaxContent "Success")
            <*> flaxContent "ResponseText"
            <*> flaxContent "State"
            <*> flaxContent "City"
            <*> flaxContent "WeatherStationCity"
            <*> readTag     "WeatherID"
            <*> flaxContent "Description"
            <*> readTag     "Temperature"
            <*> readTag     "RelativeHumidity"
            <*> flaxContent "Wind"
            <*> flaxContent "Pressure"
            <*> flaxContent "Visibility"
            <*> flaxContent "WindChill"
            <*> flaxContent "Remarks"
