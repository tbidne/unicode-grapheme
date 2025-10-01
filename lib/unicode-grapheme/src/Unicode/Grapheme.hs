-- | Unicode grapheme utilities for the latest provided unicode version.
--
-- @since 0.1
module Unicode.Grapheme
  ( -- * Motivation
    -- $motivation
    V16_0.breakGraphemeClusters,
    V16_0.textWidth,
    V16_0.clusterWidth,
  )
where

import Unicode.Grapheme.V16_0 qualified as V16_0

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
