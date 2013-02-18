-- -----------------------------------------------------------------------------

-- |
-- Module      :  Text.IPv6Addr
-- Copyright   :  (c) Michel Boucey 2011-2013
-- License     :  BSD-Style
-- Maintainer  :  michel.boucey@gmail.com
-- Stability   :  provisional
--
-- Dealing with IPv6 address text representations, canonization and manipulations.
--

-- -----------------------------------------------------------------------------

module Text.IPv6Addr.Manip
    (
      module Text.IPv6Addr.Internal
    , sixteenBitsArbToken
    , partialRandAddr
    , macAddrToIPv6AddrTokens
    , getTokIPv6AddrOf
    , getTokMacAddrOf
    ) where

import Control.Monad (replicateM)
import Data.Char (intToDigit,isHexDigit)
import Data.List (intersperse)
import Data.Maybe (catMaybes,fromJust)
import qualified Data.Text as T
import System.Random (randomRIO)

import Network.Info

import Text.IPv6Addr.Internal
import Text.IPv6Addr.Types

-- | Returns 'Just' an arbitrary 'SixteenBits' token based on a mask \"____\", each
-- underscore being replaced by a random hexadecimal digit.
--
-- > sixteenBitsArbToken "_f__" == Just (SixteenBits "bfd4")
-- 
sixteenBitsArbToken :: String -> IO (Maybe IPv6AddrToken)
sixteenBitsArbToken m =
    mapM getHex m >>= \cs -> return $ sixteenBits $ T.pack cs
  where getHex c =
            case c of
                '_' -> hexRand
                otherwise -> return c
          where hexRand = randomRIO(0,15) >>= \r -> return $ intToDigit r

-- | Generates a partial 'IPv6Addr' with n 'SixteenBits'
partialRandAddr :: Int -> IO [IPv6AddrToken]
partialRandAddr n =
    if n < 9 then do l <- replicateM n $ sixteenBitsArbToken "____"
                     return $ intersperse Colon $ catMaybes l
             else return []

-- | Given a MAC address, returns the corresponding 'IPv6AddrToken' list, or an empty list.
--
-- > macAddrToIPv6AddrTokens "fa:1d:58:cc:95:16" == [SixteenBits "fa1d",Colon,SixteenBits "58cc",Colon,SixteenBits "9516"]
--
macAddrToIPv6AddrTokens :: T.Text -> [IPv6AddrToken]
macAddrToIPv6AddrTokens mac =
    if T.length mac == 17
    then do
        let p = snd $ trans (T.split (==':') mac,[])
        if length p == 3
           then intersperse Colon $ map (fromJust . maybeIPv6AddrToken) p
           else []
    else []
  where
    trans ([],l) = ([],l)
    trans (l1,l2) = do
        let s = splitAt 2 l1
        trans (snd s,l2 ++ [T.concat $ fst s]) 

--
-- Functions based upon Network.Info to get local MAC and IPv6 addresses.
--

networkInterfacesMacAddrList :: IO [(String,MAC)]
networkInterfacesMacAddrList =
    getNetworkInterfaces >>= \n -> return $ map networkInterfacesMac n 
  where networkInterfacesMac (NetworkInterface n _ _ m) = (n,m)

-- | Given a valid name of a local network interface, returns 'Just' the list of
-- tokens of the interface's IPv6 address, or 'Nothing'.
getTokIPv6AddrOf :: String -> IO (Maybe [IPv6AddrToken])
getTokIPv6AddrOf s = do
     l <- networkInterfacesIPv6AddrList
     case lookup s l of
         Just a -> return $ maybeTokIPv6Addr $ T.pack $ show a
         Nothing -> return Nothing

-- | Given a valid name of a local network interface,
-- returns 'Just' the corresponding list of 'IPv6AddrToken' of the interface's MAC Address,
-- or 'Nothing'.
getTokMacAddrOf :: String -> IO (Maybe [IPv6AddrToken])
getTokMacAddrOf s = do
    l <- networkInterfacesMacAddrList
    case lookup s l of
        Just a -> return $ Just $ macAddrToIPv6AddrTokens $ T.pack $ show a
        Nothing -> return Nothing
