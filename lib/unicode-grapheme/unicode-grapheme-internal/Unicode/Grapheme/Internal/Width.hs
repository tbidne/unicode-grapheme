module Unicode.Grapheme.Internal.Width
  ( clusterWidth,
  )
where

import Data.HashSet qualified as HSet
import Data.Text (Text)
import Data.Text qualified as T
import Unicode.Grapheme.Internal.DB.Properties
  ( DerivedEastAsianWidth (derivedEastAsianWide),
    EmojiData (emojiPresentation),
    Properties (derivedEastAsianWidth, emojiData),
  )

clusterWidth :: Properties -> Text -> Int
clusterWidth props txt =
  if T.any isWide txt
    then 2
    else 1
  where
    isWide :: Char -> Bool
    isWide c =
      isPresentation c
        || isEastAsianWide c
        || isEmojiStyle c

    -- East_Asian_Width = Fullwidth or Wide
    isEastAsianWide c = HSet.member c eastAsianWide
    eastAsianWide = props.derivedEastAsianWidth.derivedEastAsianWide

    -- Is Emoji_Presentation
    isPresentation c = HSet.member c emojiPresentation
    emojiPresentation = props.emojiData.emojiPresentation

    -- U+FE0F indicates emoji-style "variation sequence", which seems to
    -- have width 2.
    --
    -- https://www.unicode.org/glossary/#variation_selector
    isEmojiStyle = (== '\xFE0F')
