module Unicode.Grapheme.Internal.DB.Properties
  ( Properties (..),
    DerivedCoreProperties (..),
    DerivedEastAsianWidth (..),
    EmojiData (..),
    GraphemeBreakProperties (..),
    mkCharSet,
    mkCharMap,
    mkCharBitVec,
    mkCharVecGCB,
    isProp,
    getGCB,
  )
where

import Control.Monad.ST (runST)
import Data.Bit (Bit (Bit), Vector)
import Data.Char (ord)
import Data.Char qualified as Ch
import Data.Foldable (for_)
import Data.Foldable qualified as F
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.Vector.Unboxed ((!?))
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Unicode.Grapheme.Common.DB.GraphemeClusterBreak
  ( GraphemeClusterBreak
      ( GraphemeClusterBreak_Any
      ),
  )

-- | 'Properties' is all of the properties that we care about from the Unicode
-- database. Currently it is shared between all unicode versions, hence it must
-- be a superset of each version. This is convenient as we only need 1 type,
-- and the unicode versions are close enough that this doesn't cause any
-- problems.
--
-- The only "bad fit" at the moment is that 15.0 does not have any
-- derivedCoreProperties, so its map is mempty. This is pretty minor, but if
-- new unicode versions significantly diverge, they may want their own type.
data Properties = MkProperties
  { derivedCoreProperties :: DerivedCoreProperties,
    derivedEastAsianWidth :: DerivedEastAsianWidth,
    emojiData :: EmojiData,
    graphemeBreakProperties :: GraphemeBreakProperties
  }
  deriving stock (Eq, Show)

instance Semigroup Properties where
  MkProperties a1 a2 a3 a4 <> MkProperties b1 b2 b3 b4 =
    MkProperties
      (a1 <> b1)
      (a2 <> b2)
      (a3 <> b3)
      (a4 <> b4)

instance Monoid Properties where
  mempty = MkProperties mempty mempty mempty mempty

data DerivedCoreProperties = MkDerivedCoreProperties
  { -- | Indic_Conjunct_Break=Consonant
    indicConjunctBreakConsonant :: Vector Bit,
    -- | Indic_Conjunct_Break=Extend
    indicConjunctBreakExtend :: Vector Bit,
    -- | Indic_Conjunct_Break=Linker
    indicConjunctBreakLinker :: Vector Bit
  }
  deriving stock (Eq, Show)

instance Semigroup DerivedCoreProperties where
  MkDerivedCoreProperties a1 a2 a3 <> MkDerivedCoreProperties b1 b2 b3 =
    MkDerivedCoreProperties
      (a1 <> b1)
      (a2 <> b2)
      (a3 <> b3)

instance Monoid DerivedCoreProperties where
  mempty = MkDerivedCoreProperties mempty mempty mempty

newtype DerivedEastAsianWidth = MkDerivedEastAsianWidth
  { -- | Fullwidth and Wide
    derivedEastAsianWide :: Vector Bit
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

data EmojiData = MkEmojiData
  { emojiPresentation :: Vector Bit,
    extendedPictographic :: Vector Bit
  }
  deriving stock (Eq, Show)

instance Semigroup EmojiData where
  MkEmojiData a1 a2 <> MkEmojiData b1 b2 =
    MkEmojiData
      (a1 <> b1)
      (a2 <> b2)

instance Monoid EmojiData where
  mempty = MkEmojiData mempty mempty

-- | Map for Char -> GraphemeBreakProperty.
newtype GraphemeBreakProperties = MkGraphemeBreakProperties
  { unGraphemeBreakProperties :: Vector GraphemeClusterBreak
  }
  deriving stock (Eq, Show)
  deriving newtype (Monoid, Semigroup)

mkCharSet :: [(Char, Maybe Char)] -> HashSet Char
mkCharSet = F.foldl' go HSet.empty
  where
    go acc (c, Nothing) = HSet.insert c acc
    go acc (c, Just d) = HSet.union (HSet.fromList [c .. d]) acc

mkCharBitVec :: [(Char, Maybe Char)] -> Vector Bit
mkCharBitVec xs = runST $ do
  let len = maxChar + 1
  vec <- MU.replicate len (Bit False)

  for_ xs $ \(c, mC) -> do
    for_ (toCharList (c, mC)) $ \d -> do
      MU.write vec (ord d) (Bit True)

  U.unsafeFreeze vec
  where
    maxChar = F.foldl findMax 0 xs
    findMax !acc (c, mC) = case mC of
      Nothing -> max acc (ord c)
      Just m -> max acc (ord m)

    toCharList (c, Nothing) = [c]
    toCharList (c, Just m) = [c .. m]

mkCharVecGCB :: [(Char, Maybe Char, GraphemeClusterBreak)] -> Vector GraphemeClusterBreak
mkCharVecGCB xs = runST $ do
  let len = maxChar + 1

  vec <- MU.replicate len GraphemeClusterBreak_Any

  for_ xs $ \(c, mC, gcb) -> do
    for_ (toCharList (c, mC)) $ \d -> do
      MU.write vec (ord d) gcb

  U.unsafeFreeze vec
  where
    maxChar = F.foldl findMax 0 xs
    findMax !acc (c, mC, _) = case mC of
      Nothing -> max acc (ord c)
      Just m -> max acc (ord m)

    toCharList (c, Nothing) = [c]
    toCharList (c, Just m) = [c .. m]

mkCharMap :: [(Char, Maybe Char, a)] -> HashMap Char a
mkCharMap = F.foldl' go HMap.empty
  where
    go acc (c, Nothing, x) = HMap.insert c x acc
    go acc (c, Just d, x) =
      HMap.union (HMap.fromList $ map (,x) [c .. d]) acc

isProp :: Vector Bit -> Char -> Bool
isProp vec c = case vec !? Ch.ord c of
  Nothing -> False
  Just (Bit b) -> b

getGCB :: Vector GraphemeClusterBreak -> Char -> GraphemeClusterBreak
getGCB vec c = case vec !? Ch.ord c of
  Nothing -> GraphemeClusterBreak_Any
  Just gcb -> gcb
