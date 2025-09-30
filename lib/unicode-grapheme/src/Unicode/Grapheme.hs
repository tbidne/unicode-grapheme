{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Unicode grapheme utilities.
--
-- @since 0.1
module Unicode.Grapheme
  ( -- * Motivation
    -- $motivation

    -- * Intro
    -- $intro
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
    runUnicodeFunctionLatest,

    -- * Unicode versions
    UnicodeVersion (..),

    -- ** Display
    Version.displayVersion,
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
#if MIN_VERSION_base(4, 21, 0)
import Data.Bounded (Bounded (maxBound))
#else
import GHC.Enum (Bounded (maxBound))
#endif
import Data.Foldable qualified as F
import Data.Function (const)
import Data.Functor (Functor (fmap))
import Data.Int (Int)
import Data.Monoid (Monoid (mempty))
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import Unicode.Grapheme.Internal.V14_0 qualified as V14_0
import Unicode.Grapheme.Internal.V15_0 qualified as V15_0
import Unicode.Grapheme.Internal.V15_1 qualified as V15_1
import Unicode.Grapheme.Internal.V16_0 qualified as V16_0
import Unicode.Grapheme.Internal.Version
  ( UnicodeVersion
      ( UnicodeVersion_14_0,
        UnicodeVersion_15_0,
        UnicodeVersion_15_1,
        UnicodeVersion_16_0
      ),
  )
import Unicode.Grapheme.Internal.Version qualified as Version

-- $motivation
--
-- Consider the problem of determining a string's length e.g. for display or
-- moving a cursor. The obvious choice is the basic @length@ functions
-- (@String@ has the same problems as 'Text' here, so we will stick to the
-- latter):
--
-- >>> import Data.Text qualified as T
-- >>> T.length "abc"
-- 3
--
-- But text is more than just ascii; what about characters with accents or
-- emojis?
--
-- >>> -- U+00D6
-- >>> T.length "Ö"
-- 1
--
-- >>> -- U+004F U+0308
-- >>> T.length "Ö"
-- 2
--
-- >>> -- U+1F1EF U+1F1F5
-- >>> T.length "🇯🇵"
-- 2
--
-- >>> -- 🤦🏼‍♂️ (Unicode syntax below, as ghci chokes on the literal)
-- >>> T.length "\x1F926\x1F3FC\x200D\x2642\xFE0F"
-- 5
--
-- What is going on? It turns out, both @Data.List.length@ and
-- @Data.Text.length@ count unicode
-- [/code points/](https://en.wikipedia.org/wiki/Code_point#In_Unicode),
-- not \"characters\" or width. This is a reasonable choice, but it is not
-- helpful if we want to count visual characters or visual width. The last
-- example hints at this as the single emoji is made up of 5 code points.
--
-- What we actually want to count is
-- [/grapheme clusters/](https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries),
-- and perhaps consider the visual width of each cluster.
--
-- +-------------+---------------+-------------------+--------------+
-- |        Text |   Code points | Grapheme clusters | Visual width |
-- +=============+===============+===================+==============+
-- |       @"a"@ |      @U+0097@ |                 1 |            1 |
-- +-------------+---------------+-------------------+--------------+
-- |     @"abc"@ |      @U+0097@ |                 3 |            3 |
-- |             |      @U+0098@ |                   |              |
-- |             |      @U+0099@ |                   |              |
-- +-------------+---------------+-------------------+--------------+
-- |      @"🇯🇵"@ |     @U+1F1EF@ |                 1 |            2 |
-- |             |     @U+1F1F5@ |                   |              |
-- +-------------+---------------+-------------------+--------------+
-- |   @"🤦🏼‍♂️"@ |     @U+1F926@ |                 1 |            2 |
-- |             |     @U+1F3FC@ |                   |              |
-- |             |      @U+200D@ |                   |              |
-- |             |      @U+2642@ |                   |              |
-- |             |      @U+FE0F@ |                   |              |
-- +-------------+---------------+-------------------+--------------+
--
-- The goal of this package is to provide utilities for grapheme clusters
-- and width, using only haskell dependencies.

-- $intro
--
-- Unicode functions are defined in terms of the abstract 'UnicodeFunction'
-- type, which allows us to conveniently wrap functionality across multiple
-- unicode versions. These can then be combined in a variety of ways for
-- handling the unicode version.
--
-- For example, we can use the following:
--
-- @
--   'breakGraphemeClusters' :: 'UnicodeFunction' 'Text' ['Text']
--   'breakGraphemeClusters' :: 'UnicodeFunction' a b -> a -> b
-- @
--
-- to define a function that will break the text into grapheme clusters,
-- using either @base@'s unicode version if it is supported, or falling
-- back to the latest supported version.
--
-- >>> :{
--   break :: Text -> [Text]
--   break = runUnicodeFunctionLatest breakGraphemeClusters
-- :}
--
-- Our goals with respect to unicode versions are:
--
--     1. There should be a way to select a "default" unicode, without
--        explicitly choosing a version.
--
--     2. A @base@ upgrade (hence possible unicode upgrade) should not
--        /require/ code changes i.e. cannot cause compilation or run-time
--        errors.
--
--          Note that observable behavior /can/ change with the above default
--          e.g. a newer unicode version could change a key property on some
--          code point. But a given 'UnicodeFunction' should still "work",
--          where "work" is relative to the version's semantics.

-- | Breaks 'Text' into grapheme clusters.
--
-- ==== __Examples__
--
-- >>> runUnicodeFunctionLatest breakGraphemeClusters "abc"
-- ["a","b","c"]
--
-- >>> -- U+004F U+0308
-- >>> runUnicodeFunctionLatest breakGraphemeClusters "Ö"
-- ["O\776"]
--
-- >>> -- 🧑‍🌾
-- >>> runUnicodeFunctionLatest breakGraphemeClusters "\x1F9D1\x200D\x1F33E"
-- ["\129489\8205\127806"]
--
-- @since 0.1
breakGraphemeClusters :: UnicodeFunction Text [Text]
breakGraphemeClusters =
  MkUnicodeFunction
    { v14_0 = V14_0.breakGraphemeClusters,
      v15_0 = V15_0.breakGraphemeClusters,
      v15_1 = V15_1.breakGraphemeClusters,
      v16_0 = V16_0.breakGraphemeClusters
    }

-- | Given a __single__ grapheme cluster -- of possibly multiple code points --
-- returns the width 1 or 2. This is based on heuristics i.e. if the text
-- contains at least one code point with the following properties:
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
-- >>> runUnicodeFunctionLatest clusterWidth "a"
-- 1
--
-- >>> runUnicodeFunctionLatest clusterWidth "🇯🇵"
-- 2
--
-- >>> -- Used with multiple clusters can lead to unexpected results!
-- >>> runUnicodeFunctionLatest clusterWidth "abc"
-- 1
--
-- @since 0.1
clusterWidth :: UnicodeFunction Text Int
clusterWidth =
  MkUnicodeFunction
    { v14_0 = V14_0.clusterWidth,
      v15_0 = V15_0.clusterWidth,
      v15_1 = V15_1.clusterWidth,
      v16_0 = V16_0.clusterWidth
    }

-- | Splits the text into grapheme clusters and counts each cluster width.
--
-- ==== __Examples__
--
-- >>> runUnicodeFunctionLatest textWidth "abc"
-- 3
--
-- >>> -- U+004F U+0308
-- >>> runUnicodeFunctionLatest textWidth "Ö"
-- 1
--
-- >>> -- 🧑‍🌾
-- >>> runUnicodeFunctionLatest textWidth "\x1F9D1\x200D\x1F33E"
-- 2
--
-- @since 0.1
textWidth :: UnicodeFunction Text Int
textWidth = arr F.sum . map fmap clusterWidth . breakGraphemeClusters

-- | 'UnicodeFunction' represents some function that is supported across all
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
    v14_0 :: a -> b,
    -- | @since 0.1
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
  f <> g =
    MkUnicodeFunction
      { v14_0 = \x -> f.v14_0 x <> g.v14_0 x,
        v15_0 = \x -> f.v15_0 x <> g.v15_0 x,
        v15_1 = \x -> f.v15_1 x <> g.v15_1 x,
        v16_0 = \x -> f.v16_0 x <> g.v16_0 x
      }

-- | @since 0.1
instance (Monoid b) => Monoid (UnicodeFunction a b) where
  mempty =
    MkUnicodeFunction
      { v14_0 = const mempty,
        v15_0 = const mempty,
        v15_1 = const mempty,
        v16_0 = const mempty
      }

-- | @since 0.1
instance Applicative (UnicodeFunction a) where
  pure x =
    MkUnicodeFunction
      { v14_0 = const x,
        v15_0 = const x,
        v15_1 = const x,
        v16_0 = const x
      }

  f <*> g =
    MkUnicodeFunction
      { v14_0 = \x -> f.v14_0 x (g.v14_0 x),
        v15_0 = \x -> f.v15_0 x (g.v15_0 x),
        v15_1 = \x -> f.v15_1 x (g.v15_1 x),
        v16_0 = \x -> f.v16_0 x (g.v16_0 x)
      }

-- | @since 0.1
instance Monad (UnicodeFunction a) where
  f >>= k =
    MkUnicodeFunction
      { v14_0 = \x -> (k (f.v14_0 x)).v14_0 x,
        v15_0 = \x -> (k (f.v15_0 x)).v15_0 x,
        v15_1 = \x -> (k (f.v15_1 x)).v15_1 x,
        v16_0 = \x -> (k (f.v16_0 x)).v16_0 x
      }

-- | @since 0.1
instance Category UnicodeFunction where
  id =
    MkUnicodeFunction
      { v14_0 = id,
        v15_0 = id,
        v15_1 = id,
        v16_0 = id
      }

  f . g =
    MkUnicodeFunction
      { v14_0 = f.v14_0 . g.v14_0,
        v15_0 = f.v15_0 . g.v15_0,
        v15_1 = f.v15_1 . g.v15_1,
        v16_0 = f.v16_0 . g.v16_0
      }

-- | @since 0.1
instance Arrow UnicodeFunction where
  arr f =
    MkUnicodeFunction
      { v14_0 = f,
        v15_0 = f,
        v15_1 = f,
        v16_0 = f
      }

  f *** g =
    MkUnicodeFunction
      { v14_0 = B.bimap f.v14_0 g.v14_0,
        v15_0 = B.bimap f.v15_0 g.v15_0,
        v15_1 = B.bimap f.v15_1 g.v15_1,
        v16_0 = B.bimap f.v16_0 g.v16_0
      }

-- | @since 0.1
instance ArrowApply UnicodeFunction where
  app =
    MkUnicodeFunction
      { v14_0 = \(f, x) -> f.v14_0 x,
        v15_0 = \(f, x) -> f.v15_0 x,
        v15_1 = \(f, x) -> f.v15_1 x,
        v16_0 = \(f, x) -> f.v16_0 x
      }

-- | @since 0.1
instance ArrowChoice UnicodeFunction where
  f +++ g =
    MkUnicodeFunction
      { v14_0 = B.bimap f.v14_0 g.v14_0,
        v15_0 = B.bimap f.v15_0 g.v15_0,
        v15_1 = B.bimap f.v15_1 g.v15_1,
        v16_0 = B.bimap f.v16_0 g.v16_0
      }

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
map k f =
  MkUnicodeFunction
    { v14_0 = k f.v14_0,
      v15_0 = k f.v15_0,
      v15_1 = k f.v15_1,
      v16_0 = k f.v16_0
    }

-- | Runs the 'UnicodeFunction' with the given unicode version.
--
-- >>> :{
--   runUnicodeFunction_16_0 :: UnicodeFunction a b -> a -> b
--   runUnicodeFunction_16_0 = runUnicodeFunction UnicodeVersion_16_0
-- :}
--
-- @since 0.1
runUnicodeFunction :: UnicodeVersion -> UnicodeFunction a b -> a -> b
runUnicodeFunction vers f = case vers of
  UnicodeVersion_14_0 -> f.v14_0
  UnicodeVersion_15_0 -> f.v15_0
  UnicodeVersion_15_1 -> f.v15_1
  UnicodeVersion_16_0 -> f.v16_0

-- | Runs the 'UnicodeFunction' with the latest 'UnicodeVersion'.
--
-- @since 0.1
runUnicodeFunctionLatest :: UnicodeFunction a b -> a -> b
runUnicodeFunctionLatest = runUnicodeFunction maxBound
