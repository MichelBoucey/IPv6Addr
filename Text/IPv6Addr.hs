-- | 
-- Module      :  Text.IPv6Addr
-- Copyright   :  (c) Michel Boucey 2011
-- License     :  BSD-style
-- Maintainer  :  michel.boucey@gmail.com
-- Stability   :  provisional
--
-- Dealing with IPv6 address's text representation. Main features are validation against RFC 4291 and canonization in conformation with RFC 5952.

module Text.IPv6Addr(
    IPv6Addr,
    IPv6AddrToken(..),
    -- * Validating and canonizing an IPv6 Address
    isIPv6Addr,
    maybeIPv6Addr,
    maybeTokIPv6Addr,
    maybeExpIPv6Addr,
    -- * Manipulating IPv6 address tokens
    -- ** To IPv6 Address token(s)
    maybeIPv6AddrToken,
    maybeIPv6AddrTokens,
    sixteenBitsRand,
    ipv4AddrToIPv6AddrTokens,
    -- ** Back to Text
    ipv6TokensToText,
) where

import Control.Monad (replicateM)
import Data.Char (intToDigit,isDigit,isHexDigit,toLower)
import Data.Function (on)
import Data.List (group,isSuffixOf,elemIndex,elemIndices,intersperse)
import Data.Maybe (fromJust,isJust)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Numeric (showIntAtBase)

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

-- | Some useful tokens
tokdot = T.pack "."
tokcolon = T.pack ":"
tokdcolon = T.pack "::"
tok0 = T.pack "0"
tok1 = T.pack "1"
tokffff = T.pack "ffff"
tok64 = T.pack "64"
tokff9b = T.pack "ff9b"
tokfe80 = T.pack "fe80"
tok5efe = T.pack "5efe"
tok200 = T.pack "200"

tokenizeBy :: Char -> T.Text -> [T.Text]
tokenizeBy c = T.groupBy ((==) `on` (==c))

--
-- Parsing embedded IPv4 address
--

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

--
-- Parsing IPv6 Address
--

-- | Returns an IPv4 address as an IPv6 address token.
ipv4Addr :: T.Text -> Maybe IPv6AddrToken
ipv4Addr t =
    do
        let r = map ipv4Token $ tokenizeBy '.' t
        if (Nothing `notElem` r) && (length r == 7)
            then Just (IPv4Addr t) else Nothing

colon :: T.Text -> Maybe IPv6AddrToken
colon t = if t == tokcolon then Just Colon else Nothing

doubleColon :: T.Text -> Maybe IPv6AddrToken
doubleColon t = if t == tokdcolon then Just DoubleColon else Nothing

-- | Returns a SixteenBits token.
sixteenBits:: T.Text -> Maybe IPv6AddrToken
sixteenBits t =
    if T.length t < 5 then
        do
            -- "Leading zeros MUST be suppressed" (RFC 5952, 4.1)
            let t'= T.dropWhile (=='0') t
            if T.length t' < 5 && T.all isHexDigit t'
                then
                    if T.null t'
                        then Just AllZeros
                        -- Hexadecimal digits MUST be in lowercase (RFC 5952 4.3)
                        else Just $ SixteenBits $ T.toLower t'
                else Nothing
    else Nothing

-- | Returns a random 'SixteenBits' token. E.g. sixteenBitsRand \"d\" may produce 'SixteenBits' \"d7b5\".
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

-- | Returns Just an 'IPv6Addr', or Nothing.
--
-- > maybeIPv6Addr "D045::00Da:0fA9:0:0:230.34.110.80" == Just "d045:0:da:fa9::e622:6e50"
--
maybeIPv6Addr :: T.Text -> Maybe IPv6Addr
maybeIPv6Addr t = 
     case maybeTokIPv6Addr t of
         Just a -> Just $ ipv6TokensToText a
         Nothing -> Nothing

-- | Returns Just an expanded IPv6 address, or Nothing.
maybeExpIPv6Addr :: T.Text -> Maybe IPv6Addr
maybeExpIPv6Addr t = 
     case maybeTokIPv6Addr t of
         Just a -> Just $ ipv6TokensToText $ fromDoubleColon a
         Nothing -> Nothing

-- | Returns Just one of the valid 'IPv6AddrToken', or Nothing.
maybeIPv6AddrToken :: T.Text -> Maybe IPv6AddrToken
maybeIPv6AddrToken t
    | isJust t' = t'
    | isJust(colon t) = Just Colon
    | isJust(doubleColon t) = Just DoubleColon
    | isJust(ipv4Addr t) = Just (IPv4Addr t)
    | otherwise = Nothing

    where t' = sixteenBits t

-- | Returns the corresponding 'Text' of an IPv6 address token.
ipv6TokenToText :: IPv6AddrToken -> T.Text
ipv6TokenToText (SixteenBits s) = s 
ipv6TokenToText Colon = tokcolon
ipv6TokenToText DoubleColon = tokdcolon
-- "A single 16-bit 0000 field MUST be represented as 0" (RFC 5952, 4.1)
ipv6TokenToText AllZeros = tok0
ipv6TokenToText (IPv4Addr a) = a

-- | Given an arbitrary list of 'IPv6AddrToken', returns the corresponding 'Text'.
ipv6TokensToText :: [IPv6AddrToken] -> T.Text
ipv6TokensToText l = T.concat $ map ipv6TokenToText l

-- | Returns True if a list of 'IPv6AddrToken' constitutes a valid IPv6 Address.
isIPv6Addr :: [IPv6AddrToken] -> Bool
isIPv6Addr [] = False
isIPv6Addr [DoubleColon] = True
isIPv6Addr [DoubleColon,SixteenBits tok1] = True
isIPv6Addr tks =
    diffNext tks && (do
        let cdctks = countDoubleColon tks
        let lentks = length tks
        let lasttk = last tks
        let lenconst = (lentks == 15 && cdctks == 0) || (lentks < 15 && cdctks == 1)
        firstValidToken tks &&
            (case countIPv4Addr tks of
                0 -> case lasttk of
                          SixteenBits _ -> lenconst
                          DoubleColon -> lenconst
                          AllZeros -> lenconst
                          otherwise -> False
                1 -> case lasttk of
                          IPv4Addr _ ->
                              (lentks == 13 && cdctks == 0) || (lentks < 12 && cdctks == 1)
                          otherwise -> False
                otherwise -> False))

            where

                diffNext [_] = True
                diffNext [a,a'] = a /= a'
                diffNext (a:as) = (a /= head as) && diffNext as

                firstValidToken l = 
                    case head l of
                        SixteenBits _ -> True
                        DoubleColon -> True
                        AllZeros -> True
                        otherwise -> False

                countDoubleColon l = length $ elemIndices DoubleColon l

countIPv4Addr tks =
     foldr oneMoreIPv4Addr 0 tks
     where oneMoreIPv4Addr t c =
               case t of
                   IPv4Addr _ -> c + 1
                   otherwise -> c

-- | Returns Just a list of 'IPv6AddrToken', or Nothing.
maybeIPv6AddrTokens :: T.Text -> Maybe [IPv6AddrToken]
maybeIPv6AddrTokens t = mapM maybeIPv6AddrToken (tokenizeBy ':' t)

-- | This is the main function which returns Just the list of a tokenized IPv6 address's
-- text representation validated against RFC 4291 and canonized (rewritten) in conformation
-- with RFC 5952, or Nothing.
maybeTokIPv6Addr :: T.Text -> Maybe [IPv6AddrToken]
maybeTokIPv6Addr t =
    case maybeIPv6AddrTokens t of
        Nothing -> Nothing
        Just a ->
            if isIPv6Addr a
                then Just $ (toDoubleColon . ipv4AddrReplacement . fromDoubleColon) a
            else Nothing

        where

            ipv4AddrReplacement tks =
                if ipv4AddrRewrite tks then init tks ++ ipv4AddrToIPv6AddrTokens (last tks) else tks

-- | An embedded IPv4 address have to be rewritten to output a pure IPv6 Address
-- text representation in hexadecimal digits. But some well-known prefixed IPv6
-- addresses have to keep visible in their text representation the fact that they
-- deals with IPv4 to IPv6 transition process (RFC 5952 Section 5):
--
-- IPv4-compatible IPv6 address like "::1.2.3.4"
--
-- IPv4-mapped IPv6 address like "::ffff:1.2.3.4"
--
-- IPv4-translated address like "::ffff:0:1.2.3.4"
--
-- IPv4-translatable address like "64:ff9b::1.2.3.4"
--
-- ISATAP address like "fe80::5efe:1.2.3.4"
--
ipv4AddrRewrite :: [IPv6AddrToken] -> Bool
ipv4AddrRewrite tks =
    case last tks of
        IPv4Addr _ -> do
            let itks = init tks
            not (itks == [DoubleColon]
                || itks == [DoubleColon,SixteenBits tokffff,Colon]
                || itks == [DoubleColon,SixteenBits tokffff,Colon,AllZeros,Colon]
                || itks == [SixteenBits tok64,Colon,SixteenBits tokff9b,DoubleColon]
                || [SixteenBits tok200,Colon,SixteenBits tok5efe,Colon] `isSuffixOf` itks
                || [AllZeros,Colon,SixteenBits tok5efe,Colon] `isSuffixOf` itks
                || [DoubleColon,SixteenBits tok5efe,Colon] `isSuffixOf` itks)
        otherwise -> False

-- | Rewrites Just an embedded 'IPv4Addr' into the corresponding list of pure IPv6Addr tokens, or returns an empty list.
--
-- > ipv4AddrToIPv6AddrTokens (IPv4Addr "127.0.0.1") == [SixteenBits "7f0",Colon,SixteenBits "1"]
--
ipv4AddrToIPv6AddrTokens :: IPv6AddrToken -> [IPv6AddrToken]
ipv4AddrToIPv6AddrTokens t =
    case t of
        IPv4Addr a -> do
            let m = toHex a
            [fromJust $ sixteenBits ((!!) m 0 `T.append` addZero ((!!) m 1)),Colon,
             fromJust $ sixteenBits ((!!) m 2 `T.append` addZero ((!!) m 3))]
        otherwise -> []

        where

            toHex a = map (\x -> T.pack $ showIntAtBase 16 intToDigit (read (T.unpack x)::Int) "") $ T.split (=='.') a

            addZero d = if T.length d == 1 then tok0 `T.append` d else d

fromDoubleColon :: [IPv6AddrToken] -> [IPv6AddrToken]
fromDoubleColon tks = 
    if DoubleColon `notElem` tks then tks
    else do
        let s = splitAt (fromJust $ elemIndex DoubleColon tks) tks
        let fsts = fst s
        let snds = if length(snd s) >= 1 then tail(snd s) else []
        let fste = if null fsts then [] else fsts ++ [Colon]
        let snde = if null snds then [] else Colon : snds
        fste ++ allZerosTokensReplacement(quantityOfAllZerosTokenToReplace tks) ++ snde

        where quantityOfAllZerosTokenToReplace x =
                  ntks tks - foldl (\c x -> if (x /= DoubleColon) && (x /= Colon)
                                     then c+1 else c) 0 x

                  where ntks tks = if countIPv4Addr tks == 1 then 7 else 8

              allZerosTokensReplacement x = intersperse Colon (replicate x AllZeros)

toDoubleColon :: [IPv6AddrToken] -> [IPv6AddrToken]
toDoubleColon tks =
    zerosToDoubleColon tks (zerosRunToReplace $ zerosRunsList tks)

        where

            zerosToDoubleColon :: [IPv6AddrToken] -> (Int,Int) -> [IPv6AddrToken]
            -- No all zeros token, so no double colon replacement...
            zerosToDoubleColon ls (_,0) = ls
            -- "The symbol '::' MUST NOT be used to shorten just one 16-bit 0 field" (RFC 5952 4.2.2)
            zerosToDoubleColon ls (_,1) = ls
            zerosToDoubleColon ls (i,l) =
                let ls' = filter (/= Colon) ls
                in intersperse Colon (take i ls') ++ [DoubleColon] ++ intersperse Colon (drop (i+l) ls')

            zerosRunToReplace t =
                let l = longestLengthZerosRun t
                in (firstLongestZerosRunIndex t l,l)
                    where

                        firstLongestZerosRunIndex x y =
                            sum . snd . unzip $ takeWhile (/=(True,y)) x

                        longestLengthZerosRun x =
                            maximum $ map longest x

                              where longest t = case t of
                                                    (True,i) -> i
                                                    otherwise -> 0

            zerosRunsList x = map helper $ groupZerosRuns x

                where

                    helper h =
                        if head h == AllZeros
                        then (True,lh) else (False,lh)

                        where lh = length h

                    groupZerosRuns = group . filter (/= Colon)
