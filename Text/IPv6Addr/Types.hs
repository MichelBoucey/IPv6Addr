-- | -- Module      :  Text.IPv6Addr
-- Copyright   :  (c) Michel Boucey 2011
-- License     :  BSD-style
-- Maintainer  :  michel.boucey@gmail.com
-- Stability   :  provisional
--
-- Dealing with IPv6 address's text representation.
-- Main features are validation against RFC 4291 and canonization
-- in conformation with RFC 5952.

module Text.IPv6Addr.Types where

import qualified Data.Text as T

type IPv6Addr = T.Text

data IPv6AddrToken
    = SixteenBits T.Text -- ^ A four hexadecimal digits group representing a 16-Bit chunk
    | AllZeros           -- ^ An all zeros 16-Bit chunk
    | Colon              -- ^ A separator between 16-Bit chunks
    | DoubleColon        -- ^ A double-colon stands for a unique compression of many consecutive 16-Bit chunks
    | IPv4Addr T.Text    -- ^ An embedded IPv4 address as representation of the last 32-Bit
    deriving (Eq,Show)

data IPv4AddrToken
    = Dot
    | EightBits T.Text deriving (Eq,Show)
