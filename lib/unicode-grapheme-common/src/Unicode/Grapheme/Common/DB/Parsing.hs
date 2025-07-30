module Unicode.Grapheme.Common.DB.Parsing
  ( -- * Parsing

    -- ** Code points
    parseCodePoint,
    parseCodePointRange,

    -- ** Low-level
    parseDot,
    parseSemiColon,
    parseW8NoStrip,
    parseW8,
    stringExact,

    -- * Predicates
    isDigit,

    -- * Whitespace
    stripStart,

    -- * Misc
    charRange,
    charToHexString,
    charToHexStringPadN,
    parseFirst,
    mapHexChar,
  )
where

import Control.Applicative (Alternative, asum)
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char qualified as Ch
import Data.List qualified as L
import Data.Word (Word8)

parseFirst ::
  (Alternative f, Foldable t, Functor t) =>
  t (ByteString -> f a) ->
  ByteString ->
  f a
parseFirst ps bs = asum $ ($ bs) <$> ps

-- | Parses a code point range.
--
-- @
--   0816..0819
-- @
parseCodePointRange :: ByteString -> Maybe (Char, Char, ByteString)
parseCodePointRange bs = do
  (c1, r1) <- parseCodePoint bs
  r2 <- parseDot r1
  r3 <- parseDot r2

  (c2, r4) <- parseCodePoint r3
  pure $ (c1, c2, r4)

parseDot :: ByteString -> Maybe ByteString
parseDot = parseW8NoStrip 0x2E

-- | Parses a single code point.
--
-- @
--   093A
-- @
parseCodePoint :: ByteString -> Maybe (Char, ByteString)
parseCodePoint bs = do
  guard (not . BS.null $ hex)
  pure (hexBsToChar hex, rest')
  where
    (hex, rest) = first (BS.map mapHexChar) $ BS.span isHexadecimal bs
    rest' = stripStart rest

    pows = ((\p -> 16 ^ p) <$> [0 :: Int, 1 ..])

    hexBsToChar =
      Ch.chr
        . L.foldl' (+) 0
        . L.zipWith (\p w -> p * fromIntegral w) pows
        . L.reverse
        . BS.unpack

mapHexChar :: Word8 -> Word8
mapHexChar w
  -- 0-9
  | isDigit w = (w - 0x30)
  -- A-F
  | inRange 0x41 0x46 w = (w - 0x37)
  -- a-f
  | inRange 0x61 0x66 w = (w - 0x57)
  | otherwise = w

-- ;
parseSemiColon :: ByteString -> Maybe ByteString
parseSemiColon = parseW8 0x3B

parseW8 :: Word8 -> ByteString -> Maybe ByteString
parseW8 w = fmap stripStart . parseW8NoStrip w

parseW8NoStrip :: Word8 -> ByteString -> Maybe ByteString
parseW8NoStrip w bs = do
  (b, rest) <- BS.uncons bs
  guard (b == w)
  pure rest

-- Strips prefix and trailing whitespace. Any non-whitespace immediately
-- following the prefix fails.
stringExact :: ByteString -> ByteString -> Maybe ByteString
stringExact pfx bs = do
  -- strip prefix
  r1 <- BS.stripPrefix pfx bs
  case BS.uncons r1 of
    -- 1. Nothing left: fine.
    Nothing -> pure ""
    -- 2. At least one whitespace char: fine.
    Just (0x20, r2) -> pure $ stripStart r2
    -- 3. Something else: failure. Technically we should probably check for
    -- other whitespace chars, but since this is only used in very specific
    -- spots in the unicode files, this is probably fine.
    Just (_, _) -> Nothing

stripStart :: ByteString -> ByteString
stripStart = BS.dropWhile isExactSpaceW8

isExactSpaceW8 :: (Eq a, Num a) => a -> Bool
isExactSpaceW8 w = w == 0x20

isDigit :: Word8 -> Bool
isDigit = inRange 0x30 0x39

isHexadecimal :: Word8 -> Bool
isHexadecimal w = isDigit w || isHexChar w

isHexChar :: Word8 -> Bool
isHexChar w = inRange 0x41 0x46 w || inRange 0x61 0x66 w

inRange :: Word8 -> Word8 -> Word8 -> Bool
inRange l r w = w >= l && w <= r

-- Maps a char and possible upper bound to a range.
charRange :: Char -> Maybe Char -> [Char]
charRange c Nothing = [c]
charRange c (Just d) = [c .. d]

charToHexString :: Char -> String
charToHexString = charToHexStringPadN 0

charToHexStringPadN :: Int -> Char -> String
charToHexStringPadN n = padN . L.reverse . go . fromEnum
  where
    go x = case x `divMod` 16 of
      (0, r) -> [intToHex r]
      (y, r) -> intToHex r : go y

    padN s
      | n > length s = zeroes (n - length s) ++ s
      | otherwise = s

    zeroes m = L.replicate m '0'

intToHex :: Int -> Char
intToHex i = "0123456789ABCDEF" !! i
