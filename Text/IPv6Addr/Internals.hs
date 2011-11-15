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

module Text.IPv6Addr.Internals where

import Data.Char (intToDigit,isDigit,isHexDigit,toLower)
import Data.Function (on)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text.Read (decimal)

import Text.IPv6Addr.Types

-- | Some useful tokens
tokdot = T.pack "."
tokcolon = T.pack ":"
tokdcolon = T.pack "::"
tok0 = T.pack "0"
tok4x0 = T.pack "0000"
tok1 = T.pack "1"
tokffff = T.pack "ffff"
tok64 = T.pack "64"
tokff9b = T.pack "ff9b"
tokfe80 = T.pack "fe80"
tok5efe = T.pack "5efe"
tok200 = T.pack "200"

tokenizeBy :: Char -> T.Text -> [T.Text]
tokenizeBy c = T.groupBy ((==) `on` (== c))

--
-- Validation of IPv6 adress tokens
--

dot :: T.Text -> Maybe IPv4AddrToken
dot s = if s == tokdot then Just Dot else Nothing

eightBitsToken :: T.Text -> Maybe IPv4AddrToken
eightBitsToken t =
    case decimal t of
        Right p -> do let i = fst p
                      if i >= 0 && i <= 255 && snd p == T.empty
                         then Just (EightBits t) else Nothing
        Left _ -> Nothing

ipv4Token :: T.Text -> Maybe IPv4AddrToken
ipv4Token t
    | isJust(dot t) = Just Dot
    | isJust(eightBitsToken t) = Just (EightBits t)
    | otherwise = Nothing

ipv4Addr :: T.Text -> Maybe IPv6AddrToken
ipv4Addr t = do
    let r = map ipv4Token $ tokenizeBy '.' t
    if (Nothing `notElem` r) && (length r == 7)
       then Just (IPv4Addr t) else Nothing

colon :: T.Text -> Maybe IPv6AddrToken
colon t = if t == tokcolon
             then Just Colon
             else Nothing

doubleColon :: T.Text -> Maybe IPv6AddrToken
doubleColon t = if t == tokdcolon
                   then Just DoubleColon
                   else Nothing

-- | Returns a SixteenBits token.
sixteenBits:: T.Text -> Maybe IPv6AddrToken
sixteenBits t =
    if T.length t < 5
       then do
            -- "Leading zeros MUST be suppressed" (RFC 5952, 4.1)
            let t'= T.dropWhile (=='0') t
            if T.length t' < 5 && T.all isHexDigit t'
               then if T.null t'
                       then Just AllZeros
                       -- Hexadecimal digits MUST be in lowercase (RFC 5952 4.3)
                       else Just $ SixteenBits $ T.toLower t'
               else Nothing
       else Nothing
