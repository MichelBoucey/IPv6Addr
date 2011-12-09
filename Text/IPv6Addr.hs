-- | 
-- Module      :  Text.IPv6Addr
-- Copyright   :  (c) Michel Boucey 2011-2012
-- License     :  BSD-style
-- Maintainer  :  michel.boucey@gmail.com
-- Stability   :  provisional
--
-- Dealing with IPv6 address's text representation.
-- Main features are validation against RFC 4291 and canonization
-- in conformation with RFC 5952.

module Text.IPv6Addr
    (
      IPv6Addr
    , maybeIPv6Addr
    , getIPv6AddrOf
    , maybePureIPv6Addr
    , maybeFullIPv6Addr
    ) where

import Control.Monad (replicateM)
import Data.Char (intToDigit,isDigit,isHexDigit,toLower)
import Data.Function (on)
import Data.List (group,isSuffixOf,elemIndex,elemIndices,intersperse)
import Data.Maybe (fromJust,isJust)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Numeric (showIntAtBase)

import Text.IPv6Addr.Types
import Text.IPv6Addr.Internals

-- | Returns Just the text representation of an 'IPv6Addr' validated against
-- RFC 4291 and canonized in conformation with RFC 5952, or Nothing.
--
-- > maybeIPv6Addr "D045::00Da:0fA9:0:0:230.34.110.80" == Just "d045:0:da:fa9::e622:6e50"
--
maybeIPv6Addr :: T.Text -> Maybe IPv6Addr
maybeIPv6Addr t = maybeTokIPv6Addr t >>= ipv6TokensToText

-- | Returns Just a pure 'IPv6Addr', or Nothing.
maybePureIPv6Addr :: T.Text -> Maybe IPv6Addr
maybePureIPv6Addr t = maybeTokPureIPv6Addr t >>= ipv6TokensToText

-- | Returns Just a pure and expanded IPv6 address, or Nothing.
maybeFullIPv6Addr :: T.Text -> Maybe IPv6Addr
maybeFullIPv6Addr t = do
    a <- maybeTokPureIPv6Addr t
    ipv6TokensToText $ expandTokens $ fromDoubleColon a

getIPv6AddrOf :: String -> IO (Maybe IPv6Addr)
getIPv6AddrOf s = do
     l <- networkInterfacesIPv6AddrList
     case lookup s l of
         Just a -> return $ maybeIPv6Addr $ T.pack $ show a
         Nothing -> return Nothing
