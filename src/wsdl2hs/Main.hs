{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Network.HTTP.Conduit (simpleHttp)
import           Text.XML (parseLBS_)
import           Data.Default (def)
import           Text.Show.Pretty (ppShow)
import           Options.Applicative

import           Control.Monad (when)

import           Web.SOAP.Schema (parseSchema, Schema(..))

data Opts = Opts { useFile :: Maybe String
                 , useHTTP :: Maybe String
                 , optCommand :: Command
                 } deriving (Show)

data Command = Dump
             | List
             deriving (Show)

opts :: Parser Opts
opts = Opts <$> (optional $ strOption ( short 'f' <> long "file" <> help "Load schema from a file." <> metavar "FILE"))
            <*> (optional $ strOption ( short 'h' <> long "http" <> help "Get schmea from an URL." <> metavar "URL"))
            <*> subparser ( (command "dump" $ info dumpOpts $ progDesc "Dump intermediate schema AST representation.")
                         <> (command "list" $ info listOpts $ progDesc "List schema elements.")
                          )

dumpOpts :: Parser Command
dumpOpts = pure Dump

listOpts :: Parser Command
listOpts = pure List

optParser :: ParserInfo Opts
optParser = info (helper <*> opts) (fullDesc <> progDesc "Lol" <> header "wsdl2hs - a lol program")

main :: IO ()
main = execParser optParser >>= run

run :: Opts -> IO ()
run Opts{..} = do
    doc <- case (useHTTP, useFile) of
        (Just url, Nothing)   -> parseLBS_ def `fmap` simpleHttp url
        (Nothing, Just fname) -> parseLBS_ def `fmap` BSL.readFile fname
        (Just _, Just _)      -> error "Conflicting source provided."
        (Nothing, Nothing)    -> error "No source specified."

    let schema = parseSchema doc

    case optCommand of
        Dump -> putStrLn (ppShow schema)
        List -> doList schema

doList :: Schema -> IO ()
doList = undefined
