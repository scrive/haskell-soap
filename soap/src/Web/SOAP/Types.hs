{-# LANGUAGE FlexibleInstances #-}

module Web.SOAP.Types
    ( ToNodes(..)
    , ToAttribute(..)
    , FromCursor(..)
    ) where

import Data.Text (Text)
import Text.XML
import Text.XML.Cursor (Cursor)

-- * Prepare data

-- ** Construct elements

-- | Convert data to a Node list.
--   One of the functions should be provided with others building up on it.
--   
--   Only use 'toNodes' to obtain a Node list.
class ToNodes a where
    toElement :: a -> Element
    toElement = undefined

    toElements :: a -> [Element]
    toElements x = [toElement x]

    toNodes :: a -> [Node]
    toNodes = map NodeElement . toElements

instance (ToNodes a) => ToNodes (Name, a) where
    toElement (n, b) = Element n def (toNodes b)

instance (ToNodes a) => ToNodes [a] where
    toNodes = concat . map toNodes

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
