{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Network.HTTP.Conduit (simpleHttp)
import           Text.XML (parseLBS_)
import           Data.Default (def)
import           Text.Show.Pretty (ppShow)
import           Options.Applicative

import           Control.Monad (when)

import           Web.SOAP.Schema (parseSchema)

data Opts = Opts { useFile :: Maybe String
                 , useHTTP :: Maybe String
                 , dump :: Bool
                 } deriving (Show)

opts :: Parser Opts
opts = Opts <$> (optional $ strOption ( short 'f' <> long "file" <> help "Load schema from a file." <> metavar "FILE"))
            <*> (optional $ strOption ( short 'h' <> long "http" <> help "Get schmea from an URL." <> metavar "URL"))
            <*> switch ( short 'd' <> long "dump" <> help "Dump intermediate schema AST representation.")

optParser = info (helper <*> opts) (fullDesc <> progDesc "Lol" <> header "wsdl2hs - a lol program")

main = execParser optParser >>= run

run Opts{..} = do
    doc <- case (useHTTP, useFile) of
        (Just url, Nothing)   -> parseLBS_ def `fmap` simpleHttp url
        (Nothing, Just fname) -> parseLBS_ def `fmap` BSL.readFile fname
        (Just _, Just _)      -> error "Conflicting source provided."
        (Nothing, Nothing)    -> error "No source specified."

    let schema = parseSchema doc

    when dump $ putStrLn (ppShow schema)
