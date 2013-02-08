{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import           Network.HTTP.Conduit (simpleHttp)
import           Text.XML (parseLBS_)
import           Data.Default (def)
import           Text.Show.Pretty (ppShow)
import           Options.Applicative
import           System.Directory

import           Control.Monad (when, forM_)
import           Data.List

import           Web.SOAP.Schema
import           Web.SOAP.Schema.Builder (buildCode)

-- * Arguments

data Opts = Opts { useFile :: Maybe String
                 , useHTTP :: Maybe String
                 , optCommand :: Command
                 } deriving (Show)

data Command = Dump
             | ListOps
             | Code String
             deriving (Show)

opts :: Parser Opts
opts = Opts <$> (optional $ strOption ( short 'f' <> long "file" <> help "Load schema from a file." <> metavar "FILE"))
            <*> (optional $ strOption ( short 'h' <> long "http" <> help "Get schmea from an URL." <> metavar "URL"))
            <*> subparser ( (command "dump"       $ info dumpOpts $ progDesc "Dump intermediate schema AST representation.")
                         <> (command "operations" $ info listOps  $ progDesc "List service operations.")
                         <> (command "code"       $ info codeOpts $ progDesc "Generate haskell module")
                          )

dumpOpts :: Parser Command
dumpOpts = pure Dump

listOps :: Parser Command
listOps = pure ListOps

codeOpts :: Parser Command
codeOpts = Code <$> (argument str $ metavar "PORT")

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
        Code port -> doCode schema (T.pack port)

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

-- ** Generate code

doCode :: Schema -> T.Text -> IO ()
doCode schema pname = do
    let sName = T.unpack . serviceName . schemaService $ schema
    let (mService, mOperations, mTypes) = buildCode schema pname

    needCleanup <- doesDirectoryExist "out"
    when needCleanup $ removeDirectoryRecursive "out"
    createDirectory "out"
    createDirectory "out/Web"
    createDirectory "out/Web/SOAP"
    createDirectory $ "out/Web/SOAP/" ++ sName
    createDirectory $ "out/Web/SOAP/" ++ sName ++ "/Types"
    createDirectory $ "out/Web/SOAP/" ++ sName ++ "/Ops"

    forM_ mTypes $ \(name, content) -> do
        putStrLn name
        TL.writeFile ("out/Web/SOAP/" ++ sName ++ "/Types/" ++ name ++ ".hs") content
