{-# LANGUAGE NoImplicitPrelude #-}

-- | Text grapheme utilities.
--
-- @since 0.1
module Unicode.Grapheme
  ( -- $intro
    UnicodeFunction,

    -- ** Construction
    breakGraphemeClusters,
    textWidth,
    clusterWidth,

    -- ** Operations
    dimap,
    map,

    -- ** Elimination
    runUnicodeFunction,
    runUnicodeFunctionVersion,

    -- * Unicode versions
    UnicodeVersion (..),

    -- ** Functions
    Version.getBaseUnicodeVersion,
    Version.getBaseUnicodeVersionIO,
    Version.getBaseUnicodeVersionOrLatest,

    -- ** Display
    Version.displayVersion,

    -- ** Errors
    Version.UnsupportedUnicodeE (..),
  )
where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Arrow
  ( Arrow (arr, (***)),
    ArrowApply (app),
    ArrowChoice ((+++)),
  )
import Control.Category (Category (id, (.)))
import Control.Monad (Monad ((>>=)))
import Data.Bifunctor qualified as B
import Data.Foldable qualified as F
import Data.Function (const)
import Data.Functor (Functor (fmap))
import Data.Int (Int)
import Data.Monoid (Monoid (mempty))
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import Unicode.Grapheme.Common.Version
  ( UnicodeVersion
      ( UnicodeVersion_15_0,
        UnicodeVersion_15_1,
        UnicodeVersion_16_0
      ),
  )
import Unicode.Grapheme.Common.Version qualified as Version
import Unicode.Grapheme.Internal.V15_0 qualified as V15_0
import Unicode.Grapheme.Internal.V15_1 qualified as V15_1
import Unicode.Grapheme.Internal.V16_0 qualified as V16_0

-- $intro
--
-- Unicode functions are defined in terms of the abstract 'UnicodeFunction'
-- type, which allows us to conveniently wrap functionality across multiple
-- unicode versions.
--
-- These can then be combined in a variety of ways for handling the unicode
-- version.
--
-- For example, the following function will break the text into grapheme
-- clusters, using either @base@'s unicode version if it is supported, or
-- falling back to the latest supported version.
--
-- >>> :{
--   break :: Text -> [Text]
--   break = runUnicodeFunction breakGraphemeClusters
-- :}

-- | Breaks 'Text' into grapheme clusters.
--
-- ==== __Examples__
--
-- >>> runUnicodeFunction breakGraphemeClusters "abc"
-- ["a","b","c"]
--
-- >>> -- U+004F U+0308
-- >>> runUnicodeFunction breakGraphemeClusters "Ö"
-- ["O\776"]
--
-- >>> -- 🧑‍🌾
-- >>> runUnicodeFunction breakGraphemeClusters "\x1F9D1\x200D\x1F33E"
-- ["\129489\8205\127806"]
--
-- @since 0.1
breakGraphemeClusters :: UnicodeFunction Text [Text]
breakGraphemeClusters =
  MkUnicodeFunction
    { v15_0 = V15_0.breakGraphemeClusters,
      v15_1 = V15_1.breakGraphemeClusters,
      v16_0 = V16_0.breakGraphemeClusters
    }

-- | Given a __single__ grapheme cluster -- of possibly multiple codepoints --
-- returns the width 1 or 2. This is based on heuristics i.e. if the text
-- contains at least one codepoint with the following properties:
--
--    - East_Asian_Width = Fullwidth or Wide
--    - Emoji_Presentation
--    - U+FE0F (emoji-style)
--
-- Then width is 2. Otherwise it is 1.
--
-- ===== __Examples__
--
--
-- >>> runUnicodeFunction clusterWidth "a"
-- 1
--
-- >>> runUnicodeFunction clusterWidth "🇯🇵"
-- 2
--
-- >>> -- Used with multiple codepoints can lead to unexpected results!
-- >>> runUnicodeFunction clusterWidth "abc"
-- 1
--
-- @since 0.1
clusterWidth :: UnicodeFunction Text Int
clusterWidth =
  MkUnicodeFunction
    { v15_0 = V15_0.clusterWidth,
      v15_1 = V15_1.clusterWidth,
      v16_0 = V16_0.clusterWidth
    }

-- | Splits the text into grapheme clusters and counts each cluster width.
--
-- ==== __Examples__
--
-- >>> runUnicodeFunction textWidth "abc"
-- 3
--
-- >>> -- U+004F U+0308
-- >>> runUnicodeFunction textWidth "Ö"
-- 1
--
-- >>> -- 🧑‍🌾
-- >>> runUnicodeFunction textWidth "\x1F9D1\x200D\x1F33E"
-- 2
--
-- @since 0.1
textWidth :: UnicodeFunction Text Int
textWidth = arr F.sum . map fmap clusterWidth . breakGraphemeClusters

-- | 'UnicodeFunction' represents some function that works across all
-- 'UnicodeVersion's. It can be extended via its 'Category' and 'Arrow'
-- instances.
--
-- >>> :{
--   textWidth :: UnicodeFunction Text Int
--   textWidth = arr F.sum . map fmap clusterWidth . breakGraphemeClusters
-- :}
--
-- @since 0.1
data UnicodeFunction a b = MkUnicodeFunction
  { -- | @since 0.1
    v15_0 :: a -> b,
    -- | @since 0.1
    v15_1 :: a -> b,
    -- | @since 0.1
    v16_0 :: a -> b
  }
  deriving stock
    ( -- | @since 0.1
      Functor
    )

-- | @since 0.1
instance (Semigroup b) => Semigroup (UnicodeFunction a b) where
  MkUnicodeFunction f1 f2 f3 <> MkUnicodeFunction g1 g2 g3 =
    MkUnicodeFunction
      (\x -> f1 x <> g1 x)
      (\x -> f2 x <> g2 x)
      (\x -> f3 x <> g3 x)

-- | @since 0.1
instance (Monoid b) => Monoid (UnicodeFunction a b) where
  mempty = MkUnicodeFunction (const mempty) (const mempty) (const mempty)

-- | @since 0.1
instance Applicative (UnicodeFunction a) where
  pure x = MkUnicodeFunction (const x) (const x) (const x)

  MkUnicodeFunction f1 f2 f3 <*> MkUnicodeFunction g1 g2 g3 =
    MkUnicodeFunction
      (\x -> f1 x (g1 x))
      (\x -> f2 x (g2 x))
      (\x -> f3 x (g3 x))

-- | @since 0.1
instance Monad (UnicodeFunction a) where
  MkUnicodeFunction f1 f2 f3 >>= k =
    MkUnicodeFunction
      (\x -> (k (f1 x)).v15_0 x)
      (\x -> (k (f2 x)).v15_1 x)
      (\x -> (k (f3 x)).v16_0 x)

-- | @since 0.1
instance Category UnicodeFunction where
  id = MkUnicodeFunction id id id

  MkUnicodeFunction f1 f2 f3 . MkUnicodeFunction g1 g2 g3 =
    MkUnicodeFunction
      (f1 . g1)
      (f2 . g2)
      (f3 . g3)

-- | @since 0.1
instance Arrow UnicodeFunction where
  arr f = MkUnicodeFunction f f f

  MkUnicodeFunction f1 f2 f3 *** MkUnicodeFunction g1 g2 g3 =
    MkUnicodeFunction
      (B.bimap f1 g1)
      (B.bimap f2 g2)
      (B.bimap f3 g3)

-- | @since 0.1
instance ArrowApply UnicodeFunction where
  app =
    MkUnicodeFunction
      (\(MkUnicodeFunction f1 _ _, x) -> f1 x)
      (\(MkUnicodeFunction _ f2 _, x) -> f2 x)
      (\(MkUnicodeFunction _ _ f3, x) -> f3 x)

-- | @since 0.1
instance ArrowChoice UnicodeFunction where
  MkUnicodeFunction f1 f2 f3 +++ MkUnicodeFunction g1 g2 g3 =
    MkUnicodeFunction
      (B.bimap f1 g1)
      (B.bimap f2 g2)
      (B.bimap f3 g3)

-- | Dimaps a 'UnicodeFunction'.
--
-- @since 0.1
dimap ::
  -- | Contravariantly map input.
  (c -> a) ->
  -- | Covariantly map output.
  (b -> d) ->
  UnicodeFunction a b ->
  UnicodeFunction c d
dimap f g = map (\k -> g . k . f)

-- | Maps a 'UnicodeFunction'.
--
-- @since 0.1
map ::
  -- | Function mapper.
  ((a -> b) -> c -> d) ->
  -- | Unicode function.
  UnicodeFunction a b ->
  UnicodeFunction c d
map k (MkUnicodeFunction f1 f2 f3) =
  MkUnicodeFunction
    (k f1)
    (k f2)
    (k f3)

-- | Runs the 'UnicodeFunction' with @base@'s unicode version, if it is
-- supported. Otherwise uses the latest supported version.
--
-- @since 0.1
runUnicodeFunction :: UnicodeFunction a b -> a -> b
runUnicodeFunction = runUnicodeFunctionVersion Version.getBaseUnicodeVersionOrLatest

-- | Runs the 'UnicodeFunction' with the given unicode version.
--
-- @since 0.1
runUnicodeFunctionVersion :: UnicodeVersion -> UnicodeFunction a b -> a -> b
runUnicodeFunctionVersion vers f = case vers of
  UnicodeVersion_15_0 -> f.v15_0
  UnicodeVersion_15_1 -> f.v15_1
  UnicodeVersion_16_0 -> f.v16_0
