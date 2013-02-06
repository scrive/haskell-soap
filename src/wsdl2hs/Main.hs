{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit (simpleHttp)
import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import qualified Data.Text as T

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

        name = head $ srv $| laxAttribute "name"
        ports = map port $ srv $/ laxElement "port"
        port p = Port { portName    = head $ p $| laxAttribute "name"
                      , portBinding = dropNS . head $ p $| laxAttribute "binding"
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
                           , operationDocumentation :: Maybe Text
                           , operationInput :: Text
                           , operationOutput :: Text
                           } deriving (Show)

bindings :: Cursor -> [Binding]
bindings wsdl = map binding $ wsdl $/ laxElement "binding"
    where
        binding b = Binding { bindingName = bindName b
                            , bindingType = bindType b
                            , bindingTransport = head . head $ b $/ laxElement "binding" &| laxAttribute "transport"
                            , bindingOperations = map (operation $ bindType b) $ b $/ laxElement "operation"
                            }
        bindName b = head $ b $| laxAttribute "name"
        bindType b = dropNS . head $ b $| laxAttribute "type"

        operation bt o = Operation { operationName = opName o
                                   , operationAction = head . concat $ o $/ laxElement "operation" &| laxAttribute "soapAction"
                                   , operationDocumentation = listToMaybe $ opDetails bt o $/ laxElement "documentation" &/ content

                                   , operationInput  = mPart $ dropNS . head . concat $ opDetails bt o $/ laxElement "input"  &| laxAttribute "message"
                                   , operationOutput = mPart $ dropNS . head . concat $ opDetails bt o $/ laxElement "output" &| laxAttribute "message"
                                   }

        opName o = head $ o $| laxAttribute "name"

        pType bt = head . concat $ wsdl $/ laxElement "portType" &| attributeIs "name" bt
        opDetails bt o = head . concat $ pType bt $/ laxElement "operation" &| attributeIs "name" (opName o)

        mPart n = dropNS . head . concat . concat . concat $ wsdl $/ laxElement "message" &| attributeIs "name" n
                                                                  &/ laxElement "part" &| attributeIs "name" "parameters"
                                                                  &| laxAttribute "element"

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

-- * Utils

-- | Remove tns: et al. from attribute values.
dropNS :: Text -> Text
dropNS = snd . T.breakOnEnd ":"
