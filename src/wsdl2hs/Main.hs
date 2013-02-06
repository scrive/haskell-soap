{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit (simpleHttp)
import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
--import qualified Data.Map.Lazy as ML

import Data.Maybe
import System.Environment (getArgs)

-- * Schema elements

-- ** Service / ports

data Service = Service { serviceName :: Text
                       , servicePorts :: [Port]
                       } deriving (Show)

data Port = Port { portName :: Text
                 , portBinding :: Text
                 , portAddress :: Text
                 } deriving (Show)

service :: Cursor -> Service
service wsdl = Service name ports
    where
        srv = head $ wsdl $/ laxElement "service"

        name = head $ srv $/ laxAttribute "name"
        ports = map port $ srv $/ laxElement "port"
        port p = Port { portName    = "" -- head $ p $/ laxAttribute "name"
                      , portBinding = "" -- head $ p $/ laxAttribute "binding"
                      , portAddress = head $ p $// laxAttribute "location"
                      }

-- ** Bindings / operations

data Binding = Binding { bindingName :: Text
                       , bindingType :: Text
                       , bindingTransport :: Text
                       , bindingOperations :: [Operation]
                       } deriving (Show)

data Operation = Operation { operationName :: Text
                           , operationAction :: Text
                           , operationDocumentation :: Text
                           , operationInput :: Text
                           , operationOutput :: Text
                           } deriving (Show)

bindings :: Cursor -> [Binding]
bindings wsdl = map binding $ wsdl $/ laxElement "binding"
    where
        binding b = Binding { bindingName = undefined
                            , bindingType = undefined
                            , bindingTransport = undefined
                            , bindingOperations = map operation $ undefined
                            }

        operation o = Operation { operationName = undefined
                                , operationAction = undefined
                                , operationDocumentation = undefined
                                , operationInput = undefined
                                , operationOutput = undefined
                                }

-- ** Types / fields

data SOAPType =
    ComplexType Text [ComplexField]
  | SimpleType { simpleTypeBase :: Text
               , simpleTypeEnumeration :: Maybe [Text]
               }
  deriving (Show)

data ComplexField = ComplexField { fieldMinOccurs :: Int
                                 , fieldMaxOccurs :: Int
                                 , fieldName :: Text
                                 , fieldType :: Text
                                 } deriving (Show)

types :: Cursor -> [(Text, SOAPType)]
types wsdl = map typeElement $ wsdl $/ laxElement "types" &/ laxElement "schema" &/ laxElement "element"
    where
        typeElement e = undefined

-- * Load up stuff

process url stuff = do
    wsdl <- (fromDocument . parseLBS_ def) `fmap` simpleHttp url
    print $ service wsdl
    print $ bindings wsdl
    print $ types wsdl

-- * Entry point

help = putStrLn "wsdl2hs <URL> [stuff, ...]"

main = do
    args <- getArgs
    case args of
        [] -> help
        url:stuff -> process url stuff
