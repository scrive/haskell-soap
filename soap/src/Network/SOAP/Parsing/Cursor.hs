{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Some helpers to parse documents with Text.XML.Cursor.

module Network.SOAP.Parsing.Cursor
    (
      -- * Extract single element
      readT, readC
      -- * Extract from multiple elements
    , Dict, readDict, dictBy
    ) where

import Network.SOAP (ResponseParser(CursorParser))

import Text.XML
import Text.XML.Cursor

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (mapMaybe)

-- ** Single-element extraction.

-- | Grab node content by element name.
--
-- > pair cur = (readT "fst" cur, readT "snd" cur)
readT :: Text -> Cursor -> Text
readT n c = T.concat $ c $/ laxElement n &/ content
{-# INLINE readT #-}

-- | Extract a read-able type from a content of a node with given name.
--
-- > age = readC "age" :: Cursor -> Integer
readC :: (Read a) => Text -> Cursor -> a
readC n c = read . T.unpack $ readT n c
{-# INLINE readC #-}

-- ** Multi-element extraction.

-- | Very generic type to catch server reply when you don't care about types.
type Dict = HM.HashMap Text Text

-- | Apply an axis and extract a key-value from child elements.
--
-- > invokeWS … (CursorParser . readDict $ laxElement "WebScaleResponse" &/ laxElement "BigDataResult")
readDict :: Axis -> Cursor -> Dict
readDict a c = extract . head $ c $/ a
    where
        extract cur = HM.fromList . mapMaybe dict . map node $ cur $| child

        dict (NodeElement (Element (Name n _ _) _ [NodeContent cont])) = Just (n, cont)
        dict (NodeElement (Element (Name n _ _) _ []))              = Just (n, T.empty)
        dict _                                                      = Nothing

-- | Simple parser to grab a flat response by an element name.
--
-- > result <- invokeWS … (dictBy "BigDataResult")
-- > case HM.lookup "SuccessError" result of …
dictBy :: T.Text -> ResponseParser Dict
dictBy n = CursorParser . readDict $ anyElement &/ laxElement n
