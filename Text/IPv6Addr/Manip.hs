module Text.IPv6Addr.Manip
    ( randIPv6AddrChunk
    , randPartialIPv6Addr
    , macAddrToIPv6AddrTokens
    , getTokIPv6AddrOf
    , getTokMacAddrOf
    ) where

import           Control.Monad          (replicateM)
import           Data.Attoparsec.Text   as A
import           Data.Char              (intToDigit)
import           Data.List              (intersperse)
import qualified Data.Text              as T
import           Network.Info
import           System.Random          (randomRIO)

import           Text.IPv6Addr.Internal
import           Text.IPv6Addr.Types

-- | Returns 'Just' a random 'SixteenBit' token based on a mask \"____\", each
-- underscore being replaced by a random hexadecimal digit.
--
-- > randIPv6AddrChunk "_f__" == Just (SixteenBit "bfd4")
--
randIPv6AddrChunk :: String -> IO IPv6AddrToken
randIPv6AddrChunk m =
    mapM getHex m >>= \g -> return $ SixteenBit $ T.dropWhile (=='0') $ T.pack g
  where
    getHex c
        | c == '_'  = intToDigit <$> randomRIO (0,15)
        | otherwise = return c

-- | Generates a random partial 'IPv6Addr' with n 'SixteenBit'
randPartialIPv6Addr :: Int -> IO [IPv6AddrToken]
randPartialIPv6Addr n
    | n > 0 && n < 9 = intersperse Colon <$> replicateM n (randIPv6AddrChunk "____")
    | otherwise      = return []

-- | Given a MAC address, returns 'Just' the corresponding 'IPv6AddrToken' list, or 'Nothing'.
--
-- > macAddrToIPv6AddrTokens "fa:1d:58:cc:95:16" == Just [SixteenBit "fa1d",Colon,SixteenBit "58cc",Colon,SixteenBit "9516"]
--
macAddrToIPv6AddrTokens :: T.Text -> Maybe [IPv6AddrToken]
macAddrToIPv6AddrTokens t =
    case parse macAddr t of
        Done a b -> if a == T.empty then intersperse Colon <$> b else Nothing
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

