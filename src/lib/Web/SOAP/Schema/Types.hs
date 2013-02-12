module Web.SOAP.Schema.Types where

import Data.Text (Text)
import Text.XML (Element)
import Text.XML.Cursor (Cursor)

class SOAPType a where
    soapName :: a -> Text
    soapValue :: a -> Text

class ToElements a where
    toElements :: a -> [Element]

class FromCursor a where
    fromCursor :: Cursor -> a
