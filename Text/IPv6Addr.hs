-- -----------------------------------------------------------------------------
-- | 
-- Module      :  Text.IPv6Addr
-- Copyright   :  (c) Michel Boucey 2011-2013
-- License     :  BSD-style
-- Maintainer  :  michel.boucey@gmail.com
-- Stability   :  provisional
--
-- Dealing with IPv6 address text representation,
-- canonization and manipulations.
--
-- -----------------------------------------------------------------------------

module Text.IPv6Addr
    (
      IPv6Addr
    , maybeIPv6Addr
    , maybePureIPv6Addr
    , maybeFullIPv6Addr
    , getIPv6AddrOf
    , randIPv6Addr
    ) where

import Control.Monad (replicateM)
import Data.Char (intToDigit,isDigit,isHexDigit,toLower)
import Data.Function (on)
import Data.List (group,isSuffixOf,elemIndex,elemIndices,intersperse)
import Data.Maybe (catMaybes,fromJust,isJust)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Numeric (showIntAtBase)

import Text.IPv6Addr.Internal
import Text.IPv6Addr.Manip (sixteenBitsArbToken,partialRandAddr)
import Text.IPv6Addr.Types

-- | Returns Just the text representation of a canonized
-- 'IPv6Addr' in conformation with RFC 5952, or Nothing.
--
-- > maybeIPv6Addr "0:0::FFFF:192.0.2.128" == Just "::ffff:192.0.2.128"
--
maybeIPv6Addr :: T.Text -> Maybe IPv6Addr
maybeIPv6Addr t = maybeTokIPv6Addr t >>= ipv6TokensToIPv6Addr

-- | Returns Just a pure 'IPv6Addr', or Nothing.
--
-- > maybePureIPv6Addr "::ffff:192.0.2.128" == Just "::ffff:c000:280"
--
maybePureIPv6Addr :: T.Text -> Maybe IPv6Addr
maybePureIPv6Addr t = maybeTokPureIPv6Addr t >>= ipv6TokensToIPv6Addr

-- | Returns Just a pure and expanded 'IPv6Addr', or Nothing.
--
-- > maybeFullIPv6Addr "::ffff:192.0.2.128" == Just "0000:0000:0000:0000:0000:ffff:c000:0280"
--
maybeFullIPv6Addr :: T.Text -> Maybe IPv6Addr
maybeFullIPv6Addr t =
   maybeTokPureIPv6Addr t >>= \m -> ipv6TokensToIPv6Addr $ expandTokens $ fromDoubleColon m

-- | Returns the reverse lookup domain name corresponding to the address
-- as define in RFC 3596 section 2.5.
--
-- > ip6arpa "4321:0:1:2:3:4:567:89ab" == "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.0.0.0.0.1.2.3.4.ip6.arpa."
--
-- ip6arpa :: T.Text -> Maybe T.Text

revaddr :: T.Text -> T.Text -> T.Text
revaddr i o = if i == T.empty then o
              else do let c = T.last i
                      if c /= ':'
                      then revaddr (T.init i) (o `T.append` (T.pack [c]) `T.append` T.pack ".")
                      else revaddr (T.init i) o

-- | Returns Just the canonized 'IPv6Addr' of the given network interface,
-- or Nothing.
--
-- > getIPv6AddrOf "eth0"
--
getIPv6AddrOf :: String -> IO (Maybe IPv6Addr)
getIPv6AddrOf s = do
     l <- networkInterfacesIPv6AddrList
     case lookup s l of
         Just a  -> return $ maybeIPv6Addr $ T.pack $ show a
         Nothing -> return Nothing

-- | Returns a full random 'IPv6Addr'
randIPv6Addr :: IO IPv6Addr
randIPv6Addr = partialRandAddr 8 >>= \p -> return $ IPv6Addr $ ipv6TokensToText p
