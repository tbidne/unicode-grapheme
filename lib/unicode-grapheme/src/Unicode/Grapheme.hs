{-# LANGUAGE NoImplicitPrelude #-}

-- | Text grapheme utilities.
--
-- @since 0.1
module Unicode.Grapheme
  ( -- $intro
    UnicodeFunction,
    breakGraphemeClusters,
    textWidth,
    clusterWidth,

    -- ** Version Combinators
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

import Control.Arrow (Arrow (arr, (***)))
import Control.Category (Category (id, (.)))
import Data.Foldable qualified as F
import Data.Functor (Functor (fmap))
import Data.Int (Int)
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
-- @
--   break :: 'Text' -> ['Text']
--   break = 'runUnicodeFunction' 'breakGraphemeClusters'
-- @

-- | Breaks 'Text' into grapheme clusters.
--
-- @since 0.1
breakGraphemeClusters :: UnicodeFunction Text [Text]
breakGraphemeClusters =
  MkUnicodeFunction
    { v15_0 = V15_0.breakGraphemeClusters,
      v15_1 = V15_1.breakGraphemeClusters,
      v16_0 = V16_0.breakGraphemeClusters
    }

-- | Given a __single__ grapheme cluster, returns the width 1 or 2. This
-- is based on heuristics i.e. if the text contains at least one codepoint
-- with the following properties:
--
--    - East_Asian_Width = Fullwidth or Wide
--    - Emoji_Presentation
--    - U+FE0F (emoji-style)
--
-- Then width is 2. Otherwise it is 1.
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
-- @since 0.1
textWidth :: UnicodeFunction Text Int
textWidth = arr F.sum . liftUF clusterWidth . breakGraphemeClusters

-- | 'UnicodeFunction' represents some function that works across all
-- 'UnicodeVersion's.
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
      (\(x, y) -> (f1 x, g1 y))
      (\(x, y) -> (f2 x, g2 y))
      (\(x, y) -> (f3 x, g3 y))

liftUF :: (Functor f) => UnicodeFunction a b -> UnicodeFunction (f a) (f b)
liftUF (MkUnicodeFunction f1 f2 f3) =
  MkUnicodeFunction
    (fmap f1)
    (fmap f2)
    (fmap f3)

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
