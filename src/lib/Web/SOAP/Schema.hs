{-# LANGUAGE OverloadedStrings #-}

module Web.SOAP.Schema
    ( parseSchema
    , parseService
    , parseBindings
    , parseTypes
    ) where

import           Text.XML
import           Text.XML.Cursor
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe

-- * Schema elements

data Schema = Schema { schemaService :: Service
                     , schemaBindings :: [Binding]
                     , schemaTypes :: [(Text, SOAPType)]
                     } deriving (Show)

parseSchema :: Document -> Schema
parseSchema doc = Schema (parseService wsdl) (parseBindings wsdl) (parseTypes wsdl)
    where wsdl = fromDocument doc

-- ** Service / ports

data Service = Service { serviceName :: Text
                       , servicePorts :: [Port]
                       } deriving (Show)

data Port = Port { portName :: Text
                 , portBinding :: Text
                 , portAddress :: Text
                 } deriving (Show)

parseService :: Cursor -> Service
parseService wsdl = Service name ports
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

parseBindings :: Cursor -> [Binding]
parseBindings wsdl = map binding $ wsdl $/ laxElement "binding"
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
    ComplexType [ComplexField]
  | SimpleType { simpleTypeBase :: Text
               , simpleTypeEnumeration :: Maybe [Text]
               }
  deriving (Show)

data ComplexField = ComplexField { fieldMinOccurs :: Int
                                 , fieldMaxOccurs :: Int
                                 , fieldName :: Text
                                 , fieldType :: Text
                                 } deriving (Show)

parseTypes :: Cursor -> [(Text, SOAPType)]
parseTypes wsdl = concat [ map typeElement $ wsdl $/ laxElement "types" &/ laxElement "schema" &/ laxElement "element"
                    , map typeComplex $ wsdl $/ laxElement "types" &/ laxElement "schema" &/ laxElement "complexType"
                    , map typeSimple  $ wsdl $/ laxElement "types" &/ laxElement "schema" &/ laxElement "simpleType"
                    ]
    where
        typeElement e = (eName e, eType e)
        typeComplex e = (eName e, teComplex e)
        typeSimple e  = (eName e, teSimple e)

        eName e = head $ e $| laxAttribute "name"

        eType e = case ( e $/ laxElement "complexType"
                       , e $/ laxElement "simpleType"
                       ) of
                       (complex:_, _) -> teComplex complex
                       (_, simple:_) -> teSimple simple
                       oops -> error $ "unknown element type: " ++ show oops

        teComplex c = ComplexType $ map teComplexField $ c $/ laxElement "sequence" &/ laxElement "element"
        teComplexField f = ComplexField { fieldMinOccurs = read . T.unpack . head $ f $| laxAttribute "minOccurs"
                                        , fieldMaxOccurs = read . T.unpack . head $ f $| laxAttribute "maxOccurs"
                                        , fieldName = head $ f $| laxAttribute "name"
                                        , fieldType = head $ f $| laxAttribute "type"
                                        }

        teSimple s = SimpleType { simpleTypeBase = stBase s
                                , simpleTypeEnumeration = stEnum s
                                }
        stBase s = head . concat $ s $/ laxElement "restriction" &| laxAttribute "base"
        stEnum s = squash . map stEnumValue $ s $/ laxElement "restriction" &/ laxElement "enumeration"
        stEnumValue v = head $ v $| laxAttribute "value"
        squash [] = Nothing
        squash vs = Just vs

-- * Parsing utils

-- | Remove tns: et al. from attribute values.
dropNS :: Text -> Text
dropNS = snd . T.breakOnEnd ":"
