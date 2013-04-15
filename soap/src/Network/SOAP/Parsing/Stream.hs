-- | Collection of helpers to use with Text.XML.Stream.Parse parsers.
--
-- > let sink = flaxTag "MethodNameResponse"
-- >          $ flaxTag "MethodNameResult" $ do
-- >              info <- flaxTag "Info" $ do
-- >                          q <- readTag "quantity"
-- >                          b <- readTag "balance"
-- >                          return $ Info q b
-- >              rc <- readTag "ResponseCode"
-- >              return (rc, info)

module Network.SOAP.Parsing.Stream
    ( -- * Tags
      laxTag, flaxTag
      -- * Content
    , readContent, readTag
      -- * Types to use in custom parser sinks
    , Sink, Event
    ) where

import Data.Conduit
import Data.XML.Types (Event)

import           Text.XML (Name(..))
import qualified Text.XML.Stream.Parse as XSP

import           Data.Text (Text, unpack)

-- | Namespace- and attribute- ignorant tagNoAttr.
laxTag :: (MonadThrow m) => Text -> Sink Event m a -> Sink Event m (Maybe a)
laxTag ln = XSP.tagPredicate ((== ln) . nameLocalName) XSP.ignoreAttrs . const

-- | Non-maybe version of laxTag/tagNoAttr.
flaxTag :: (MonadThrow m) => Text -> Sink Event m a -> Sink Event m a
flaxTag ln s = XSP.force ("got no " ++ show ln) $ laxTag ln s

-- | Unpack and read a current tag content.
readContent :: (Read a, MonadThrow m) => Sink Event m a
readContent = fmap (read . unpack) XSP.content

-- | Unpack and read tag content by local name.
readTag :: (Read a, MonadThrow m) => Text -> Sink Event m a
readTag n = flaxTag n readContent
