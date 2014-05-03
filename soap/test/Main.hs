{-# LANGUAGE OverloadedStrings #-}
import Network.SOAP
import Network.SOAP.Exception
import Network.SOAP.Parsing.Cursor
import Network.SOAP.Parsing.Stream
import qualified Network.SOAP.Transport.Mock as Mock
import qualified Network.SOAP.Transport.HTTP as HTTP
import Text.XML
import Text.XML.Writer
import Text.XML.Cursor as Cur hiding (element)
import Text.XML.Stream.Parse as Parse

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as LBS

import Test.Hspec

main = hspec $ do
    describe "Transport.Mock" $ do
        it "dispatches requests" $ do
            t <- Mock.initTransport [ ("ping", const $ return "pong") ]
            result <- t "ping" (document "request" empty)
            result `shouldBe` "pong"

        it "generates a soap response" $ do
            t <- Mock.initTransport [ ("foo", Mock.handler $ \_ -> return ())]
            result <- t "foo" (document "request" empty)
            result `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"><soapenv:Body/></soapenv:Envelope>"

    context "SOAP" $ do
        it "Smoke-test with RawParser" $ do
            t <- Mock.initTransport [ ("ping", const $ return "pong") ]
            result <- invokeWS t "ping" () () (RawParser id)
            result `shouldBe` "pong"

        describe "CursorParser" $ do
            let salad cur = head $ cur $/ laxElement "salad"

            let check parser = do
                t <- Mock.initTransport [ ("spam", saladHandler )]
                invokeWS t "spam" () () parser

            it "reads content" $ do
                result <- check $ CursorParser (readT "bacon" . salad)
                result `shouldBe` "many"

            it "reads and converts" $ do
                result <- check $ CursorParser (readC "eggs" . salad)
                result `shouldBe` (2 :: Integer)

            it "reads dict" $ do
                result <- check $ CursorParser (readDict $ laxElement "salad" )
                result `shouldBe` HM.fromList [ ("bacon","many")
                                              , ("sausage","some")
                                              , ("eggs","2")
                                              ]
        describe "StreamParser" $ do
            it "extracts stuff" $ do
                let recipeParser = do
                    ings <- Parse.force "no salad" $ Parse.tagNoAttr "salad" $ Parse.many $ Parse.tag Just return $ \name -> do
                        quantity <- Parse.content
                        return $ RecipeEntry (nameLocalName name) quantity
                    return $ Recipe ings

                t <- spamTransport
                result <- invokeWS t "spam" () () $ StreamParser recipeParser
                result `shouldBe` saladRecipe

            it "extracts using lax helpers" $ do
                let recipeParser = flaxTag "salad" $ do
                    s <- flaxContent "sausage"
                    b <- laxContent "bacon"
                    e <- readTag "eggs"
                    return $ Recipe [ RecipeEntry "sausage" s
                                    , RecipeEntry "bacon" $ maybe "" id b
                                    , RecipeEntry "eggs" . T.pack $ show (e :: Int)
                                    ]
                result <- invokeSpam $ StreamParser recipeParser
                result `shouldBe` saladRecipe

        describe "DocumentParser" $ do
            it "gives out raw document" $ do
                let poach doc = read . T.unpack . T.concat
                                     $ fromDocument doc
                                     $// laxElement "eggs"
                                     &/ Cur.content
                t <- spamTransport
                result <- invokeWS t "spam" () () $ DocumentParser poach
                result `shouldBe` (2 :: Int)

        describe "Exception" $ do
            it "parses a SOAP Fault document" $ do
                t <- Mock.initTransport [ ("crash", Mock.fault "soap:Server" "The server made a boo boo." "") ]
                lbs <- t "crash" (document "request" empty)
                let Just e = extractSoapFault . parseLBS_ def $ lbs
                e `shouldBe` SOAPFault { faultCode = "soap:Server"
                                       , faultString = "The server made a boo boo."
                                       , faultDetail = ""
                                       }

invokeSpam parser = do
    t <- spamTransport
    invokeWS t "spam" () () parser

spamTransport = Mock.initTransport [ ("spam", saladHandler) ]

saladHandler = Mock.handler $ \_ -> do
    return . element "salad" $ do
        element "sausage" ("some" :: Text)
        element "bacon" ("many" :: Text)
        element "eggs" (2 :: Integer)

data RecipeEntry = RecipeEntry Text Text deriving (Eq, Show)
data Recipe = Recipe [RecipeEntry] deriving (Eq, Show)

saladRecipe = Recipe [ RecipeEntry "sausage" "some"
                     , RecipeEntry "bacon" "many"
                     , RecipeEntry "eggs" "2"
                     ]
