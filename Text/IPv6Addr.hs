-- -----------------------------------------------------------------------------

-- Module      :  Text.IPv6Addr
-- Copyright   :  Copyright © Michel Boucey 2011-2015
-- License     :  BSD-style
-- Maintainer  :  michel.boucey@gmail.com
--
-- Dealing with IPv6 address text representations, canonization and manipulations.
--

-- -----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Text.IPv6Addr
    (
      IPv6Addr (IPv6Addr)
    , maybeIPv6Addr
    , maybePureIPv6Addr
    , maybeFullIPv6Addr
    , sameIPv6Addr
    , fromIPv6Addr
    , toIPv6
    , toHostName
    , toIP6ARPA
    , getIPv6AddrOf
    , randIPv6Addr
    , randIPv6AddrWithPrefix
    ) where

import Control.Applicative (pure,(<$>),(<*>))
import Data.IP (IPv6)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Network (HostName)
import System.Random (randomRIO)

import Text.IPv6Addr.Internal
import Text.IPv6Addr.Manip (randIPv6AddrChunk,randPartialIPv6Addr)
import Text.IPv6Addr.Types

instance Eq IPv6Addr where
    (==) (IPv6Addr a) (IPv6Addr b) = show (maybePureIPv6Addr a) == show (maybePureIPv6Addr b)

-- | Returns 'Just' the text representation of a canonized
-- 'IPv6Addr' in conformation with RFC 5952, or 'Nothing'.
--
-- > maybeIPv6Addr "0:0::FFFF:192.0.2.128" == Just (IPv6Addr "::ffff:192.0.2.128")
--
maybeIPv6Addr :: T.Text -> Maybe IPv6Addr
maybeIPv6Addr t = maybeTokIPv6Addr t >>= ipv6TokensToIPv6Addr

-- | Returns 'Just' a pure 'IPv6Addr', or 'Nothing'.
--
-- > maybePureIPv6Addr "::ffff:192.0.2.128" == Just (IPv6Addr "::ffff:c000:280")
--
maybePureIPv6Addr :: T.Text -> Maybe IPv6Addr
maybePureIPv6Addr t = maybeTokPureIPv6Addr t >>= ipv6TokensToIPv6Addr

-- | Returns 'Just' a pure and fully expanded 'IPv6Addr', or 'Nothing'.
--
-- > maybeFullIPv6Addr "::ffff:192.0.2.128" == Just (IPv6Addr "0000:0000:0000:0000:0000:ffff:c000:0280")
--
maybeFullIPv6Addr :: T.Text -> Maybe IPv6Addr
maybeFullIPv6Addr t = maybeTokPureIPv6Addr t >>= (ipv6TokensToIPv6Addr . expandTokens . fromDoubleColon)

-- | Returns 'True' if arguments are two textual representations of a same IPv6 address.
sameIPv6Addr :: T.Text -> T.Text -> Bool
sameIPv6Addr a b =
    case maybePureIPv6Addr a of
        Nothing -> False
        Just a' -> case maybePureIPv6Addr b of
                       Nothing -> False
                       Just b' -> a' == b'

-- | Returns the reverse lookup domain name corresponding of the given IPv6 address (RFC 3596 Section 2.5).
--
-- > toIP6ARPA (IPv6Addr "4321:0:1:2:3:4:567:89ab") == "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.0.0.0.0.1.2.3.4.IP6.ARPA."
--
toIP6ARPA :: IPv6Addr -> T.Text
toIP6ARPA a =
    T.append (T.reverse $ T.concatMap trans $ fromIPv6Addr $ fromJust $ maybeFullIPv6Addr $ fromIPv6Addr a) "IP6.ARPA."
  where
    trans ':' = T.empty
    trans c   = T.append "." (T.pack [c])

-- | Given an 'IPv6Addr', returns the corresponding 'HostName'.
toHostName :: IPv6Addr -> HostName
toHostName = show

-- | Given an 'IPv6addr', returns the corresponding 'IPv6' address.
toIPv6 :: IPv6Addr -> IPv6
toIPv6 a = read $ show a

-- | Returns 'Just' the canonized 'IPv6Addr' of the given local network interface,
-- or 'Nothing'.
--
-- > getIPv6AddrOf "eth0"
--
getIPv6AddrOf :: String -> IO (Maybe IPv6Addr)
getIPv6AddrOf s = maybe Nothing (maybeIPv6Addr . T.pack . show) <$> (lookup s <$> networkInterfacesIPv6AddrList)

-- | Returns a random 'IPv6Addr'.
randIPv6Addr :: IO IPv6Addr
randIPv6Addr = do
    r   <- randomRIO (1,8)
    tks <- case r of
              8 -> randPartialIPv6Addr 8
              _ -> do r' <- randomRIO (1,8-r)
                      case r + r' of
                          7 -> concat <$> sequence [randPartialIPv6Addr r,pure [Colon,AllZeros,Colon],randPartialIPv6Addr r']
                          8 -> randPartialIPv6Addr 8
                          _ -> concat <$> sequence [randPartialIPv6Addr r,pure [DoubleColon],randPartialIPv6Addr r']
    return $ fromJust $ ipv6TokensToIPv6Addr tks

-- | Returns a random 'IPv6Addr' with the given prefix
randIPv6AddrWithPrefix :: T.Text -> IO (Maybe IPv6Addr)
randIPv6AddrWithPrefix t =
    if t == T.empty
        then do r <- randIPv6Addr
                return $ Just r
        else case maybeIPv6AddrTokens t of
                 Just tks -> do
                     ntks <- do
                         let ctks = countChunks tks
                         case snd ctks of
                             0 -> return $ 8 - fst ctks
                             1 -> return $ 6 - fst ctks
                             _ -> return 0
                     if ntks > 0
                         then do
                             rtks <- randPartialIPv6Addr ntks
                             let tks' = addColon tks ++ rtks
                             return $ if isIPv6Addr tks'
                                          then ipv6TokensToIPv6Addr $ (toDoubleColon . fromDoubleColon) tks'
                                          else Nothing
                         else return Nothing
                 Nothing  -> return Nothing
  where
    countChunks =
        foldr count (0,0)
      where
        count c (a,b) =
            case c of
                SixteenBit _ -> (a+1,b)
                AllZeros     -> (a+1,b)
                DoubleColon  -> (a,b+1)
                _            -> (a,b)
    addColon ts =
        case last ts of
            SixteenBit _ -> ts ++ [Colon]
            AllZeros     -> ts ++ [Colon]
            _            -> ts 
