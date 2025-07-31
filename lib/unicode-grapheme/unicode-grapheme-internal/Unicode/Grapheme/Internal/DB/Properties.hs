module Unicode.Grapheme.Internal.DB.Properties
  ( Properties (..),
    DerivedCoreProperties (..),
    EmojiData (..),
    GraphemeBreakProperties (..),
    mkCharSet,
    mkCharMap,
  )
where

import Data.Foldable qualified as F
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Unicode.Grapheme.Common.DB.GraphemeClusterBreak (GraphemeClusterBreak)

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
    emojiData :: EmojiData,
    graphemeBreakProperties :: GraphemeBreakProperties
  }
  deriving stock (Eq, Show)

instance Semigroup Properties where
  MkProperties a1 a2 a3 <> MkProperties b1 b2 b3 =
    MkProperties
      (a1 <> b1)
      (a2 <> b2)
      (a3 <> b3)

instance Monoid Properties where
  mempty = MkProperties mempty mempty mempty

data DerivedCoreProperties = MkDerivedCoreProperties
  { -- | Indic_Conjunct_Break=Consonant
    indicConjunctBreakConsonant :: HashSet Char,
    -- | Indic_Conjunct_Break=Extend
    indicConjunctBreakExtend :: HashSet Char,
    -- | Indic_Conjunct_Break=Linker
    indicConjunctBreakLinker :: HashSet Char
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

newtype EmojiData = MkEmojiData
  { extendedPictographic :: HashSet Char
  }
  deriving stock (Eq, Show)
  deriving newtype (Monoid, Semigroup)

-- | Map for Char -> GraphemeBreakProperty.
newtype GraphemeBreakProperties = MkGraphemeBreakProperties
  { unGraphemeBreakProperties :: HashMap Char GraphemeClusterBreak
  }
  deriving stock (Eq, Show)
  deriving newtype (Monoid, Semigroup)

mkCharSet :: [(Char, Maybe Char)] -> HashSet Char
mkCharSet = F.foldl' go HSet.empty
  where
    go acc (c, Nothing) = HSet.insert c acc
    go acc (c, Just d) = HSet.union (HSet.fromList [c .. d]) acc

mkCharMap :: [(Char, Maybe Char, a)] -> HashMap Char a
mkCharMap = F.foldl' go HMap.empty
  where
    go acc (c, Nothing, x) = HMap.insert c x acc
    go acc (c, Just d, x) =
      HMap.union (HMap.fromList $ map (,x) [c .. d]) acc
