-- |
-- Module      :  Text.IPv6Addr
-- Copyright   :  (c) Michel Boucey 2011
-- License     :  BSD-style
-- Maintainer  :  michel.boucey@gmail.com
-- Stability   :  provisional
--
-- Dealing with IPv6 address's text representation.
-- Main features are validation against RFC 4291 and canonization
-- in conformation with RFC 5952.

module Text.IPv6Addr.Utils
    (
      sixteenBitsArbToken
    , macAddrToIPv6AddrTokens
    , getIPv6AddrOf
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

import Text.IPv6Addr
import Text.IPv6Addr.Internals
import Text.IPv6Addr.Types

-- | Returns an arbitrary 'SixteenBits' token based on a mask \"____\", each
-- underscore being replaced by a random hexadecimal digit.
--
-- > sixteenBitsArbToken "_f__" == Just (SixteenBits "bfd4")
-- 
sixteenBitsArbToken :: String -> IO (Maybe IPv6AddrToken)
sixteenBitsArbToken m = do
    cs <- mapM getHex m
    return $ sixteenBits $ T.pack cs
  where getHex c =
            case c of
                '_' -> hexRand
                otherwise -> return c
          where hexRand = do
                    r <- randomRIO(0,15)
                    return $ intToDigit r

ipv6AddrRand :: IO (Maybe IPv6Addr)
ipv6AddrRand = do
    l <- replicateM 8 (sixteenBitsArbToken "____")
    return $ ipv6TokensToText $ intersperse Colon $ catMaybes l

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
-- Functions based upon Network.Info to get local MAC and IPv6 adresses.
--

networkInterfacesMacAddrList :: IO [(String,MAC)]
networkInterfacesMacAddrList = do
    n <- getNetworkInterfaces
    return $ map networkInterfacesMac n 
  where networkInterfacesMac (NetworkInterface n _ _ m) = (n,m)

-- | Given a valid name of a local network interface, returns Just the list of
-- tokens of the interface's IPv6 address.
getTokIPv6AddrOf :: String -> IO (Maybe [IPv6AddrToken])
getTokIPv6AddrOf s = do
     l <- networkInterfacesIPv6AddrList
     case lookup s l of
         Just a -> return $ maybeTokIPv6Addr $ T.pack $ show a
         Nothing -> return Nothing

-- | Given the valid name of a local network interface,
-- returns the corresponding list of 'IPv6AddrToken' of the interface's MAC Address.
getTokMacAddrOf :: String -> IO (Maybe [IPv6AddrToken])
getTokMacAddrOf s = do
    l <- networkInterfacesMacAddrList
    case lookup s l of
        Just a -> return $ Just $ macAddrToIPv6AddrTokens $ T.pack $ show a
        Nothing -> return Nothing
