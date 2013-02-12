{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Web.SOAP.Schema.Builder
    ( buildCode
    ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder

import           Data.Monoid
import           Data.Maybe

import           Web.SOAP.Schema

-- * Module builders

buildCode :: Schema -> T.Text -> (TL.Text, TL.Text, [(String, TL.Text)])
buildCode schema portName = ( toLazyText "module %PREFIX%.%ServiceName%.Service where"
                            , toLazyText "module %PREFIX%.%ServiceName%.Operations where"
                            , [ (T.unpack name, toLazyText $ typeModule schema name type_)
                              | (name, type_) <- schemaTypes schema
                              ]
                            )

-- ** Types

typeModule :: Schema -> T.Text -> SOAPType -> Builder
typeModule schema tn t =
    mconcat [ modHeader schema $ "Types." <> tn
            , "import qualified Data.Text as T\n\n"
            , "import           Web.SOAP.Schema.Types\n"
            , modImports t
            , case t of
                  ComplexType fs              -> buildComplex tn fs
                  SimpleType base (Just enum) -> buildSimpleEnum tn base enum
                  x                           -> error $ "Unknown field spec: " ++ show x
            ]
    where
        modImports :: SOAPType -> Builder
        modImports (ComplexType fs) = mconcat . catMaybes . map cfDeps $ fs
        modImports _ = ""

        cfDeps :: ComplexField -> Maybe Builder
        cfDeps cf = case T.breakOnEnd ":" (fieldType cf) of
                        ("tns:", cfType) -> Just $ typeImport schema cfType
                        ("s:", _) -> Nothing

buildComplex :: T.Text -> [ComplexField] -> Builder
buildComplex tn fs =
    mconcat [ mconcat [ "data ", fromText tn, " = ", fromText tn, " { " ]
            , mconcat [ buildField pos f
                      | (pos, f) <- zip [0..] fs
                      ]
            , spaces indent
            , "} deriving (Show, Read, Eq)\n\n"
            ]
    where
        buildField pos f@(ComplexField{..}) =
            mconcat [ justify pos
                    , fromText $ abbr tn
                    , fromText $ title fieldName
                    , " :: "
                    , recordType fieldType
                    , singleton '\n'
                    ]

        indent = T.length tn * 2 + 9

        justify 0 = ""
        justify _ = spaces indent <> ", "

recordType :: T.Text -> Builder
recordType "s:int" = "Int"
recordType "s:long" = "Int"
recordType "s:string" = "T.Text"
recordType "s:decimal" = "Float"
recordType "s:boolean" = "Bool"
recordType "s:dateTime" = "String" -- TODO: use a real type
recordType (T.breakOnEnd ":" -> (tns, tn)) =
    case tns of
        "tns:" -> fromText tn
        x -> error $ "Unknown type: " ++ show (tns <> tn)

buildSimpleEnum tn base enum =
    mconcat [ mconcat [ "{- base = ", fromText base ," -}\n" ]
            , mconcat [ "data ", fromText tn, " = " ]
            , mconcat [ justify pos <> fromText e <> singleton '\n'
                      | (pos, e) <- zip [0..] enum
                      ]
            , mconcat [ spaces indent, "deriving (Show, Read, Eq)\n\n" ]
            ]

    where
        indent = T.length tn + 6

        justify 0 = ""
        justify _ = spaces indent <> "| "

-- * Builder utils

modName :: Schema -> T.Text -> Builder
modName schema name = mconcat [ "Web.SOAP."
                              , fromText (serviceName . schemaService $ schema)
                              , singleton '.'
                              , fromText name
                              ]

modHeader :: Schema -> T.Text -> Builder
modHeader schema name = mconcat [ "module "
                                , modName schema name
                                , " where\n\n"
                                ]

typeImport :: Schema -> T.Text -> Builder
typeImport schema name = mconcat [ "import "
                                 , modName schema $ "Types." <> name
                                 , singleton '\n'
                                 ]

spaces :: Int -> Builder
spaces n = fromString . take n $ repeat ' '

abbr :: T.Text -> T.Text
abbr = T.toLower . T.filter (flip elem ['A'..'Z'])

title :: T.Text -> T.Text
title t =
    let (head, tail) = T.splitAt 1 t
    in T.toUpper head <> tail
