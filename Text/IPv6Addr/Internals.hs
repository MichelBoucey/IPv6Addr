-- | 
-- Module      :  Text.IPv6Addr
-- Copyright   :  (c) Michel Boucey 2011
-- License     :  BSD-style
-- Maintainer  :  michel.boucey@gmail.com
-- Stability   :  provisional
--
-- Dealing with IPv6 address's text representation. Main features are validation against RFC 4291 and canonization in conformation with RFC 5952.

module Text.IPv6Addr.Internals where

import Data.Char (intToDigit,isDigit,isHexDigit,toLower)
import Data.Function (on)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text.Read (decimal)

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
-- Parsing embedded IPv4 address
--

data IPv4AddrToken
    = Dot
    | EightBits T.Text deriving (Eq,Show)

dot :: T.Text -> Maybe IPv4AddrToken
dot s = if s == tokdot then Just Dot else Nothing

eightBitsToken :: T.Text -> Maybe IPv4AddrToken
eightBitsToken t =
    case decimal t of
        Right p -> do let i = fst p
                      if i >= 0 && i <= 255 && snd p == T.empty
                          then Just (EightBits t) 
                          else Nothing
        Left _ -> Nothing

ipv4Token :: T.Text -> Maybe IPv4AddrToken
ipv4Token t
    | isJust(dot t) = Just Dot
    | isJust(eightBitsToken t) = Just (EightBits t)
    | otherwise  = Nothing
