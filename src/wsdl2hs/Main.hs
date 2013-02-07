{--# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text.Lazy.IO as TL
import           Text.XML (parseLBS_, parseText_)
import           Data.Default (def)

import           System.Environment (getArgs)
import           Text.Show.Pretty (ppShow)

import           Web.SOAP.Schema (parseSchema)

main = do
    args <- getArgs
    case args of
        "-u":url:stuff -> (parseLBS_ def `fmap` simpleHttp url) >>= process stuff
        "-f":fname:stuff -> (parseText_ def `fmap` TL.readFile fname) >>= process stuff
        _ -> help

process stuff doc =
    putStrLn . ppShow $ parseSchema doc

help = putStrLn "wsdl2hs -u <URL> | -f <FILE>"
