{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Conduit (simpleHttp)
import           Text.XML (parseLBS_)
import           Data.Default (def)
import           Text.Show.Pretty (ppShow)
import           Options.Applicative

import           Control.Monad (when, forM_)

import           Web.SOAP.Schema

-- * Arguments

data Opts = Opts { useFile :: Maybe String
                 , useHTTP :: Maybe String
                 , optCommand :: Command
                 } deriving (Show)

data Command = Dump
             | ListOps
             deriving (Show)

opts :: Parser Opts
opts = Opts <$> (optional $ strOption ( short 'f' <> long "file" <> help "Load schema from a file." <> metavar "FILE"))
            <*> (optional $ strOption ( short 'h' <> long "http" <> help "Get schmea from an URL." <> metavar "URL"))
            <*> subparser ( (command "dump"       $ info dumpOpts $ progDesc "Dump intermediate schema AST representation.")
                         <> (command "operations" $ info listOps  $ progDesc "List service operations.")
                          )

dumpOpts :: Parser Command
dumpOpts = pure Dump

listOps :: Parser Command
listOps = pure ListOps

optParser :: ParserInfo Opts
optParser = info (helper <*> opts) (fullDesc <> progDesc "Lol" <> header "wsdl2hs - a lol program")

-- * Entry

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
        Dump    -> putStrLn (ppShow schema)
        ListOps -> doList schema

-- * Commands

-- ** List operations

doList :: Schema -> IO ()
doList (Schema (Service sn ps) bs ts) = do
    putTexts ["╭┅ ", sn]

    forM_ (rEnum bs) $ \(bi, Binding bn bt btr bops) -> do
        putTexts [node bi, "───┬╼ ", bn, " <", bt, "> ", soapTransport btr]
        forM_ (rEnum bops) $ \(oi, Operation{..}) -> do
            putTexts [bar bi, "   ", node oi, "───┬╼ ", operationName]

            when (operationName /= operationAction) $
                putTexts [bar bi, "   ", bar oi, "   ├──┄ ", operationAction]

            case operationDocumentation of
                Just doc -> putTexts [bar bi, "   ", bar oi, "   ├──“", doc, "”"]
                Nothing -> return ()

            putTexts [bar bi, "   ", bar oi, "   ╘══╍ ", operationInput, " →  ", operationOutput]

putTexts :: [T.Text] -> IO ()
putTexts = T.putStrLn . T.concat

rEnum :: [a] -> [(Int, a)]
rEnum stuff = zip [l, l-1 .. 0] stuff
  where l = length stuff - 1

node :: Int -> T.Text
node 0 = "╰"
node _ = "├"

bar :: Int -> T.Text
bar 0 = " "
bar _ = "│"

soapTransport :: T.Text -> T.Text
soapTransport "http://schemas.xmlsoap.org/soap/http" = "[HTTP]"
soapTransport x = x
