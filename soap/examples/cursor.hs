{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T

import           Network.SOAP (invokeWS, Transport, ResponseParser(CursorParser))
import           Network.SOAP.Transport.HTTP (initTransport)
import           Network.SOAP.Parsing.Cursor (Dict, dictBy, readT, readC)
-- import           Text.XML.Cursor (Cursor, ($//), laxElement)
import           Text.XML.Cursor hiding (element)
import           Text.XML.Writer (elementA, element)

main :: IO ()
main = do
    transport <- initTransport "http://wsf.cdyne.com/WeatherWS/Weather.asmx" id id
    response <- getWeather transport (T.pack "10007")
    print response

getWeather :: Transport -> Text -> IO Dict
getWeather t zip = invokeWS t url () body parser -- (CursorParser altParser)
  where
    url = "http://ws.cdyne.com/WeatherWS/GetCityWeatherByZIP"
    body = elementA "GetCityWeatherByZIP" [("xmlns", "http://ws.cdyne.com/WeatherWS/")]
         $ element "ZIP" zip

    -- There are times when you just don't care...
    parser = dictBy "GetCityWeatherByZIPResult"

    -- And sometimes you care a little more...
    altParser :: Cursor -> Maybe (Int, Text)
    altParser cur = let result  = head $ cur $// laxElement "GetCityWeatherByZIPResult"
                        success = readT "Success"     result == "true"
                        temp    = readC "Temperature" result
                        descr   = readT "Description" result
                    in if success
                           then Just (temp, descr)
                           else Nothing
