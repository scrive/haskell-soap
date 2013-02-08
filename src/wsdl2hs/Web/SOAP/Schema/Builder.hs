{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Web.SOAP.Schema.Builder
    ( buildCode
    ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder

import           Data.Monoid

import           Web.SOAP.Schema

-- * Module builders

buildCode :: Schema -> T.Text -> (TL.Text, TL.Text, TL.Text)
buildCode schema portName = ( toLazyText "module %PREFIX%.%ServiceName%.Service where"
                            , toLazyText "module %PREFIX%.%ServiceName%.Operations where"
                            , toLazyText $ typesModule schema
                            )

-- ** Types

typesModule :: Schema -> Builder
typesModule schema =
    mconcat [ modHeader schema "Types"
            , "import qualified Data.Text as T\n\n"
            , mconcat (map buildType $ schemaTypes schema)
            ]

buildType :: (T.Text, SOAPType) -> Builder

buildType (tn, ComplexType fs) =
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

        recordType "s:int" = "Int"
        recordType "s:string" = "T.Text"
        recordType "s:decimal" = "Float"
        recordType (T.breakOnEnd ":" -> (tns, tn)) =
            case tns of
                "tns:" -> fromText tn
                x -> error $ "Unknown type: " ++ show (tns <> tn)

buildType (tn, SimpleType base (Just enum)) =
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

spaces :: Int -> Builder
spaces n = fromString . take n $ repeat ' '

abbr :: T.Text -> T.Text
abbr = T.toLower . T.filter (flip elem ['A'..'Z'])

title :: T.Text -> T.Text
title t =
    let (head, tail) = T.splitAt 1 t
    in T.toUpper head <> tail
