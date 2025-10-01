-- | Unicode grapheme utilities for unicode 16.0.
--
-- @since 0.1
module Unicode.Grapheme.V16_0
  ( V16_0.breakGraphemeClusters,
    textWidth,
    V16_0.clusterWidth,
  )
where

import Data.Foldable qualified as F
import Data.Text (Text)
import Unicode.Grapheme.Internal.V16_0 qualified as V16_0

-- | Splits the text into grapheme clusters and counts each cluster width.
--
-- ==== __Examples__
--
-- >>> textWidth "abc"
-- 3
--
-- >>> -- U+004F U+0308
-- >>> textWidth "Ö"
-- 1
--
-- >>> -- 🧑‍🌾
-- >>> textWidth "\x1F9D1\x200D\x1F33E"
-- 2
--
-- @since 0.1
textWidth :: Text -> Int
textWidth = F.sum . fmap V16_0.clusterWidth . V16_0.breakGraphemeClusters
