{-# LANGUAGE OverloadedStrings #-}
import Network.SOAP
import Network.SOAP.Transport.HTTP.Conduit
import Text.XML.Writer
import Text.XML.Stream.Parse as Parse
import Data.Text (Text, unpack)

main = do
    -- Initial one-time preparations.
    transport <- initTransport_ "http://localhost:10000/soap/endpoint"

    -- Making queries
    activeStaff <- listStaff transport True
    print activeStaff

-- Making queries
data Person = Person Text Int deriving Show

listStaff :: Transport -> Bool -> IO [Person]
listStaff t active = invokeWS t "urn:dummy:listStaff" () body parser
    where
        body = element "request" $ element "listStaff" $ do
                   element "active" $ toXML active
                   element "order" "age"
                   element "limit" $ toXML (10 :: Int)

        parser = StreamParser $ force "no people" $ tagNoAttr "people" $ Parse.many parsePerson

        parsePerson = tagName "person" (requireAttr "age") $ \age -> do
                          name <- Parse.content
                          return $ Person name (read . unpack $ age)
