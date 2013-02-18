{-# LANGUAGE FlexibleInstances #-}

module Web.SOAP.Schema.Types
    ( ToNodes(..)
    , ToAttribute(..)
    , FromCursor(..)
    ) where

import Data.Text (Text)
import Text.XML
import Text.XML.Cursor (Cursor)

-- * Prepare data

-- ** Construct elements

class ToNodes a where
    toElement :: a -> Element
    toElement = undefined

    toElements :: a -> [Element]
    toElements x = [toElement x]

    toNodes :: a -> [Node]
    toNodes = map NodeElement . toElements

instance (ToNodes a) => ToNodes (Name, a) where
    toElement (n, b) = Element n def (toNodes b)

instance ToNodes (Name, Text) where
    toNodes (n, c) = [NodeElement $ Element n def [NodeContent c]]

instance ToNodes Text where
    toNodes x = [NodeContent x]

-- ** Construct attributes

class ToAttribute a where
    toAttribute :: a -> (Name, Text)

--instance (ToAttribute a) => (Name, Text) where
--    toAttribute (n, t) = (n, t)

-- * Extract data from XML cursor

class FromCursor a where
    fromCursor :: Cursor -> a
