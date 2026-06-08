-- | Unicode grapheme utilities for unicode 17.0.
--
-- @since 0.1
module Unicode.Grapheme.V17_0
  ( V17_0.breakGraphemeClusters,
    textWidth,
    V17_0.clusterWidth,
  )
where

import Data.Foldable qualified as F
import Data.Text (Text)
import Unicode.Grapheme.Internal.V17_0 qualified as V17_0

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
textWidth = F.sum . fmap V17_0.clusterWidth . V17_0.breakGraphemeClusters
