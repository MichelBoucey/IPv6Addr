-- | -- Module      :  Text.IPv6Addr
-- Copyright   :  (c) Michel Boucey 2011
-- License     :  BSD-style
-- Maintainer  :  michel.boucey@gmail.com
-- Stability   :  provisional
--
-- Dealing with IPv6 address's text representation. Main features are validation against RFC 4291 and canonization in conformation with RFC 5952.

module Text.IPv6Addr.Utils where

import Data.List (group,isSuffixOf,elemIndex,elemIndices,intersperse)
import Data.Maybe (fromJust,isJust)
import qualified Data.Text as T
import Network.Info
import System.Random (randomRIO)
import Text.IPv6Addr

-- | Returns a random 'SixteenBits' token. E.g. sixteenBitsRand \"d\" may produ
ce 'SixteenBits' \"d7b5\".
sixteenBitsRand :: String -> IO IPv6AddrToken
sixteenBitsRand s =
    if all isHexDigit s && l < 4
       then do
           a <- replicateM (4-l) hexRand
           return $ SixteenBits $ T.toLower $ T.pack $ s ++ a
       else return $ SixteenBits tok0
    where
        l = length s
        hexRand = do r <- randomRIO(0,15)
                     return $ intToDigit r

-- | Given a MAC address, returns the corresponding 'IPv6AddrToken' list, or an empty list.
--
-- > macAddrToIPv6AddrTokens "fa:1d:58:cc:95:16" == [SixteenBits "fa1d",Colon,SixteenBits "58cc",Colon,SixteenBits "9516"]
--
macAddrToIPv6AddrTokens :: T.Text -> [IPv6AddrToken]
macAddrToIPv6AddrTokens mac =
      if T.length mac == 17 then do
           let p = snd $ trans (T.split (==':') mac,[])
           if length p == 3 then
                  intersperse Colon $ map (fromJust . maybeIPv6AddrToken) p
               else []
           else []

       where trans ([],l) = ([],l)
             trans (l1,l2) = do let s = splitAt 2 l1
                                trans (snd s,l2 ++ [T.concat $ fst s]) 

--
-- Functions based upon Network.Info to get local MAC and IPv6 adresses.
--

networkInterfacesIPv6AddrList :: IO [(String,IPv6)]
networkInterfacesIPv6AddrList = do
    n <- getNetworkInterfaces
    return $ map networkInterfacesIPv6Addr n 
    where networkInterfacesIPv6Addr (NetworkInterface n _ a _) = (n,a)

networkInterfacesMacAddrList :: IO [(String,MAC)]
networkInterfacesMacAddrList = do
    n <- getNetworkInterfaces
    return $ map networkInterfacesMac n 
    where networkInterfacesMac (NetworkInterface n _ _ m) = (n,m)

-- | Given a valid name of a local network interface, e.g. getIPv6AddrOf \"eth0\", return Just the interface's IPv6 address.
getIPv6AddrOf :: String -> IO (Maybe IPv6Addr)
getIPv6AddrOf s = do
     l <- networkInterfacesIPv6AddrList
     case lookup s l of
         Just a -> return $ maybeIPv6Addr $ T.pack $ show a
         Nothing -> return Nothing

-- | Given a valid name of a local network interface, returns Just the list of tokens of the interface's IPv6 address.
getTokIPv6AddrOf :: String -> IO (Maybe [IPv6AddrToken])
getTokIPv6AddrOf s = do
     l <- networkInterfacesIPv6AddrList
     case lookup s l of
         Just a -> return $ maybeTokIPv6Addr $ T.pack $ show a
         Nothing -> return Nothing

-- | Given the valid name of a local network interface, returns the corresponding list of 'IPv6AddrToken' of the interface's MAC Address.
getTokMacAddrOf :: String -> IO (Maybe [IPv6AddrToken])
getTokMacAddrOf s = do
    l <-networkInterfacesMacAddrList
    case lookup s l of
        Just a -> return $ Just $ macAddrToIPv6AddrTokens $ T.pack $ show a
        Nothing -> return Nothing
