-- -----------------------------------------------------------------------------

-- Module      :  Text.IPv6Addr
-- Copyright   :  Copyright © Michel Boucey 2011-2014
-- License     :  BSD-style
-- Maintainer  :  michel.boucey@gmail.com
--
-- Dealing with IPv6 address text representations, canonization and manipulations.
--

-- -----------------------------------------------------------------------------

module Text.IPv6Addr
    (
      IPv6Addr (IPv6Addr)
    , canonicalIPv6Addr
    , pureIPv6Addr
    , fullIPv6Addr
    , getIPv6AddrOf
    , ip6arpa
    , randIPv6Addr
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import qualified Data.Text as T

import Text.IPv6Addr.Internal
import Text.IPv6Addr.Manip (sixteenBitArbToken,partialRandAddr)
import Text.IPv6Addr.Types

-- | Returns 'Just' the text representation of a canonized
-- 'IPv6Addr' in conformation with RFC 5952, or 'Nothing'.
--
-- > canonicalIPv6Addr "0:0::FFFF:192.0.2.128" == Just (IPv6Addr "::ffff:192.0.2.128")
--
canonicalIPv6Addr :: T.Text -> Maybe IPv6Addr
canonicalIPv6Addr t = maybeTokIPv6Addr t >>= ipv6TokensToIPv6Addr

-- | Returns 'Just' a pure 'IPv6Addr', or 'Nothing'.
--
-- > pureIPv6Addr "::ffff:192.0.2.128" == Just (IPv6Addr "::ffff:c000:280")
--
pureIPv6Addr :: T.Text -> Maybe IPv6Addr
pureIPv6Addr t = maybeTokPureIPv6Addr t >>= ipv6TokensToIPv6Addr

-- | Returns 'Just' a pure and expanded 'IPv6Addr', or 'Nothing'.
--
-- > fullIPv6Addr "::ffff:192.0.2.128" == Just (IPv6Addr "0000:0000:0000:0000:0000:ffff:c000:0280")
--
fullIPv6Addr :: T.Text -> Maybe IPv6Addr
fullIPv6Addr t =
    maybeTokPureIPv6Addr t >>= (ipv6TokensToIPv6Addr . expandTokens . fromDoubleColon)

-- | Returns the reverse lookup domain name corresponding of the given IPv6 address (RFC 3596 Section 2.5).
--
-- > ip6arpa (IPv6Addr "4321:0:1:2:3:4:567:89ab") == "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.0.0.0.0.1.2.3.4.IP6.ARPA."
--
ip6arpa :: IPv6Addr -> T.Text
ip6arpa t =
    rev (fromIPv6Addr $ fromJust $ fullIPv6Addr $ fromIPv6Addr t) T.empty
  where
    rev i o = if i == T.empty
                  then o `T.append` T.pack "IP6.ARPA."
                  else do let c = T.last i
                          rev (T.init i)
                              (if c /= ':'
                                   then o `T.append` T.pack [c] `T.append` T.pack "."
                                   else o)

-- | Returns 'Just' the canonized 'IPv6Addr' of the given network interface,
-- or 'Nothing'.
--
-- > getIPv6AddrOf "eth0"
--
getIPv6AddrOf :: String -> IO (Maybe IPv6Addr)
getIPv6AddrOf s =
    maybe Nothing (canonicalIPv6Addr . T.pack . show) <$> (lookup s <$> networkInterfacesIPv6AddrList)

-- | Returns a random 'IPv6Addr'
randIPv6Addr :: IO IPv6Addr
randIPv6Addr = IPv6Addr . ipv6TokensToText <$> partialRandAddr 8
