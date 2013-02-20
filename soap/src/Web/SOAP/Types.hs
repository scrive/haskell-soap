{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Web.SOAP.Types
    ( ToNodes(..), (.=:), (.=)
    , FromCursor(..), readT, readC, readContent, Dict, asDict
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.XML
import           Text.XML.Cursor
import qualified Data.HashMap.Strict as HM
import           Data.Maybe

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

instance ToNodes () where
    toNodes () = []

instance ToNodes Text where
    toNodes x = [NodeContent x]

instance ToNodes [Node] where
    toNodes = id

instance (ToNodes a) => ToNodes (Name, a) where
    toElement (n, b) = Element n def (toNodes b)

instance (ToNodes a) => ToNodes [a] where
    toNodes = concat . map toNodes

(.=:) :: Name -> [Node] -> [Node]
n .=: ns = toNodes (n, ns)

(.=) :: Name -> Text -> Node
n .= t = head $ toNodes (n, toNodes t)

-- * Extract data from XML cursor

class FromCursor a where
    fromCursor :: Cursor -> a

-- ** Single-element extraction.

readT :: Text -> Cursor -> Text
readT n c = T.concat $ c $/ laxElement n &/ content
{-# INLINE readT #-}

readC :: (Read a) => Text -> Cursor -> a
readC n c = read . T.unpack $ readT n c
{-# INLINE readC #-}

readContent :: (Read a) => Cursor -> a
readContent = error . show . T.unpack . T.concat . content

-- ** Multi-element extraction.

type Dict = HM.HashMap Text Text

asDict :: Axis -> Cursor -> Dict
asDict a c = fromCursor . head $ c $// a

instance FromCursor Dict where
    fromCursor cur = HM.fromList . mapMaybe dict . map node $ cur $| child

dict :: Node -> Maybe (Text, Text)
dict (NodeElement (Element (Name n _ _) _ [NodeContent c])) = Just (n, c)
dict (NodeElement (Element (Name n _ _) _ []))              = Just (n, T.empty)
dict _                                                      = Nothing
{-# INLINE dict #-}

instance (FromCursor a, FromCursor b) => FromCursor (a, b) where
    fromCursor c = (fromCursor c, fromCursor c)

instance (FromCursor a, FromCursor b, FromCursor c) => FromCursor (a, b, c) where
    fromCursor c = (fromCursor c, fromCursor c, fromCursor c)
