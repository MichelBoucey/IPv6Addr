-- -----------------------------------------------------------------------------

-- |
-- Module      :  Text.IPv6Addr
-- Copyright   :  (c) Michel Boucey 2011-2013
-- License     :  BSD-Style
-- Maintainer  :  michel.boucey@gmail.com
--
-- Dealing with IPv6 address text representations, canonization and manipulations.
--

-- -----------------------------------------------------------------------------

module Text.IPv6Addr.Manip
    ( sixteenBitArbToken
    , partialRandAddr
    , macAddrToIPv6AddrTokens
    , getTokIPv6AddrOf
    , getTokMacAddrOf
    ) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Attoparsec.Text as A
import Data.Char (intToDigit,isHexDigit)
import Data.List (intersperse)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Network.Info
import System.Random (randomRIO)

import Text.IPv6Addr.Internal
import Text.IPv6Addr.Types

-- | Returns 'Just' an arbitrary 'SixteenBit' token based on a mask \"____\", each
-- underscore being replaced by a random hexadecimal digit.
--
-- > sixteenBitArbToken "_f__" == Just (SixteenBit "bfd4")
-- 
sixteenBitArbToken :: String -> IO IPv6AddrToken
sixteenBitArbToken m =
    mapM getHex m >>= \g -> return $ SixteenBit $ T.dropWhile (=='0') $ T.pack g
  where
    getHex c
        | c == '_'  = randomRIO(0,15) >>= \r -> return $ intToDigit r
        | otherwise = return c

-- | Generates a partial 'IPv6Addr' with n 'SixteenBit'
partialRandAddr :: Int -> IO [IPv6AddrToken]
partialRandAddr n
    | n > 0 && n < 9 = intersperse Colon <$> replicateM n (sixteenBitArbToken "____")
    | otherwise = return []

-- | Given a MAC address, returns the corresponding 'IPv6AddrToken' list, or an empty list.
--
-- > macAddrToIPv6AddrTokens "fa:1d:58:cc:95:16" == [SixteenBit "fa1d",Colon,SixteenBit "58cc",Colon,SixteenBit "9516"]
--
macAddrToIPv6AddrTokens :: T.Text -> Maybe [IPv6AddrToken]
macAddrToIPv6AddrTokens t =
    case parse macAddr t of
        Done a b -> if a==T.empty then intersperse Colon <$> b else Nothing
        _        -> Nothing

--
-- Functions based upon Network.Info to get local MAC and IPv6 addresses.
--
-- | Given a valid name of a local network interface, returns 'Just' the list of
-- tokens of the interface's IPv6 address, or 'Nothing'.
--
-- > getTokIPv6AddrOf "eth0" == Just [SixteenBit "fe80",DoubleColon,SixteenBit "fa1d",Colon,SixteenBit "58cc",Colon,SixteenBit "9516"]
--
getTokIPv6AddrOf :: String -> IO (Maybe [IPv6AddrToken])
getTokIPv6AddrOf s = maybe Nothing (maybeTokIPv6Addr. T.pack . show) <$> (lookup s <$> networkInterfacesIPv6AddrList)

-- | Given a valid name of a local network interface,
-- returns 'Just' the corresponding list of 'IPv6AddrToken' of the interface's MAC Address,
-- or 'Nothing'.
--
-- > getTokMacAddrOf "eth0" == Just [SixteenBit "fa1d",Colon,SixteenBit "58cc",Colon,SixteenBit "9516"]
--
getTokMacAddrOf :: String -> IO (Maybe [IPv6AddrToken])
getTokMacAddrOf s =
    maybe Nothing (macAddrToIPv6AddrTokens . T.pack . show) <$> (lookup s <$> networkInterfacesMacAddrList)
  where
    networkInterfacesMacAddrList = getNetworkInterfaces >>= \n -> return $ map networkInterfacesMac n 
      where networkInterfacesMac (NetworkInterface n _ _ m) = (n,m)
