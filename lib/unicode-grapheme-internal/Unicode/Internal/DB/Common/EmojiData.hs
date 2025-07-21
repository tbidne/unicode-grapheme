{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Unicode.Internal.DB.Common.EmojiData
  ( -- * Types
    EmojiDataI (..),
    EmojiData,
    EmojiDataAssertions,

    -- * Creation
    readUnicodeDataIO,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (Exception (displayException), throwIO)
import Control.Monad (unless)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.Kind (Type)
import Language.Haskell.TH.Syntax (Lift)
import System.File.OsPath qualified as FileIO
import System.OsPath (osp)
import Unicode.Internal.DB.Common.Utils
  ( PropertiesF,
    PropertiesIndex (PropertiesAssertions, PropertiesData),
  )
import Unicode.Internal.DB.Common.Utils qualified as Common.Utils
import Unicode.Internal.DB.Utils qualified as DB.Utils
import Unicode.Internal.Version (UnicodeVersion)

type EmojiDataI :: PropertiesIndex -> Type
newtype EmojiDataI p = MkEmojiDataI
  { extendedPictographic :: PropertiesF p
  }

instance (Semigroup (PropertiesF p)) => Semigroup (EmojiDataI p) where
  MkEmojiDataI a1 <> MkEmojiDataI b1 =
    MkEmojiDataI
      (a1 <> b1)

instance (Monoid (PropertiesF p)) => Monoid (EmojiDataI p) where
  mempty = MkEmojiDataI mempty

-- | Type synonym for EmojiDataI data.
type EmojiData = EmojiDataI PropertiesData

-- | Type synonym for EmojiDataI assertions.
type EmojiDataAssertions = EmojiDataI PropertiesAssertions

deriving stock instance (Eq (PropertiesF p)) => Eq (EmojiDataI p)

deriving stock instance (Lift (PropertiesF p)) => Lift (EmojiDataI p)

deriving stock instance (Show (PropertiesF p)) => Show (EmojiDataI p)

-- | Emoji properties we care about.
data EmojiDataProperty
  = Extended_Pictographic
  deriving stock (Eq, Show)

-- | Reads emoji data corresponding to the given unicode version.
readUnicodeDataIO ::
  EmojiDataAssertions ->
  UnicodeVersion ->
  IO EmojiData
readUnicodeDataIO asserts uvers = do
  bs <- FileIO.readFile' path
  let ls = C8.lines bs
      props = foldMap lineToDerivedProps ls

  checkAsserts props

  pure props
  where
    path =
      Common.Utils.mkUnicodePath uvers [osp|emoji-data.txt|]

    checkAsserts :: EmojiData -> IO ()
    checkAsserts cats = do
      checkAssert
        Extended_Pictographic
        asserts.extendedPictographic
        cats.extendedPictographic

    checkAssert :: EmojiDataProperty -> Int -> HashSet Char -> IO ()
    checkAssert propType expected catsSet = do
      let actual = length catsSet
      unless (expected == actual) $
        throwIO $
          MkEmojiDataE
            { propType,
              expected,
              actual
            }

lineToDerivedProps :: ByteString -> EmojiData
lineToDerivedProps bs = maybe mempty f (bsToProp bs)
  where
    f (p, c, mC) = case p of
      Extended_Pictographic ->
        MkEmojiDataI
          { extendedPictographic = HSet.fromList cs
          }
      where
        cs = DB.Utils.charRange c mC

-- | Parses a bytestring to a unicode property and char range.
bsToProp :: ByteString -> Maybe (EmojiDataProperty, Char, Maybe Char)
bsToProp bs = do
  (c1, mC2, r1) <-
    first Just <$> DB.Utils.parseCodePointRange bs
      <|> (\(c, b) -> (c, Nothing, b)) <$> DB.Utils.parseCodePoint bs

  r2 <- DB.Utils.parseSemiColon r1

  (prop, _) <- parseEmojiDataProperty r2

  pure $ (prop, c1, mC2)

parseEmojiDataProperty :: ByteString -> Maybe (EmojiDataProperty, ByteString)
parseEmojiDataProperty =
  DB.Utils.parseFirst
    [ pExtended_Pictographic
    ]
  where
    pExtended_Pictographic =
      mkCons Extended_Pictographic
        . BS.stripPrefix "Extended_Pictographic#"

    mkCons ::
      EmojiDataProperty ->
      Maybe ByteString ->
      Maybe (EmojiDataProperty, ByteString)
    mkCons c = fmap ((c,) . DB.Utils.stripStart)

data EmojiDataE = MkEmojiDataE
  { propType :: EmojiDataProperty,
    expected :: Int,
    actual :: Int
  }
  deriving stock (Eq, Show)

instance Exception EmojiDataE where
  displayException ex =
    mconcat
      [ "Emoji_Data property parse '",
        show ex.propType,
        "' failure. Expected ",
        show ex.expected,
        ", received ",
        show ex.actual
      ]
