module Unicode.Grapheme.Internal.V15_1
  ( breakGraphemeClusters,
    breakGraphemeClustersRules,
    breakGraphemeClustersStates,
    database,

    -- * Rules
    rules,
    gb1,
    gb2,
    gb3,
    gb4,
    gb5,
    gb6,
    gb7,
    gb8,
    gb9,
    gb9a,
    gb9b,
    gb9c,
    gb11,
    gb12_13,
  )
where

import Control.Monad (guard)
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Vector.Strict ((!), (!?))
import Unicode.Grapheme.Common.DB.GraphemeClusterBreak
  ( GraphemeClusterBreak
      ( GraphemeClusterBreak_CR,
        GraphemeClusterBreak_Extend,
        GraphemeClusterBreak_L,
        GraphemeClusterBreak_LF,
        GraphemeClusterBreak_LV,
        GraphemeClusterBreak_LVT,
        GraphemeClusterBreak_Prepend,
        GraphemeClusterBreak_Regional_Indicator,
        GraphemeClusterBreak_SpacingMark,
        GraphemeClusterBreak_T,
        GraphemeClusterBreak_V,
        GraphemeClusterBreak_ZWJ
      ),
  )
import Unicode.Grapheme.Internal.ClusterState
  ( ClusterOutput (ClusterBreak, ClusterChar),
    ClusterState (MkClusterState, clusters, input, inputIdx, lastRule),
    Clusters (MkClusters, unClusters),
    Rule (MkRule),
    RulesMatched,
    assertChar,
    graphemeBreakProperty,
    mkSimpleRule,
    (∈),
  )
import Unicode.Grapheme.Internal.ClusterState qualified as ClusterState
import Unicode.Grapheme.Internal.DB.Properties
  ( DerivedCoreProperties
      ( indicConjunctBreakConsonant,
        indicConjunctBreakExtend,
        indicConjunctBreakLinker
      ),
    EmojiData (extendedPictographic),
    Properties (derivedCoreProperties, emojiData),
  )
import Unicode.Grapheme.Internal.V15_1.DB
  ( UnicodeDatabase (unUnicodeDatabase),
    database,
  )

breakGraphemeClusters :: Text -> [Text]
breakGraphemeClusters =
  ClusterState.breakGraphemeClusters database rules

breakGraphemeClustersRules :: Text -> (RulesMatched, [Text])
breakGraphemeClustersRules =
  ClusterState.breakGraphemeClustersRules database rules

breakGraphemeClustersStates :: Text -> Seq ClusterState
breakGraphemeClustersStates =
  ClusterState.breakGraphemeClustersStates database rules

-- https://www.unicode.org/reports/tr29/tr29-43.html

rules :: [Rule UnicodeDatabase]
rules =
  [ gb1,
    gb2,
    gb3,
    gb4,
    gb5,
    gb6,
    gb7,
    gb8,
    gb9,
    gb9a,
    gb9b,
    gb9c,
    gb11,
    gb12_13
  ]

gb1 :: Rule UnicodeDatabase
gb1 = MkRule $ \_ state -> do
  guard $ state.inputIdx == 0
  guard $ state.clusters.unClusters == Empty

  case state.input !? 0 of
    Nothing -> Nothing
    Just c ->
      pure $
        MkClusterState
          { -- NOTE: This rule is called [0.2] in the test file.
            lastRule = Just "GB1",
            clusters = MkClusters $ Seq.singleton (ClusterChar c),
            input = state.input,
            inputIdx = 1
          }

gb2 :: Rule UnicodeDatabase
gb2 = MkRule $ \_ state -> do
  guard $ state.inputIdx == length state.input
  guard $ state.clusters.unClusters /= Empty

  pure $
    MkClusterState
      { -- NOTE: This rule is called [0.3] in the test file.
        lastRule = Just "GB2",
        clusters = state.clusters,
        input = state.input,
        inputIdx = state.inputIdx + 1
      }

gb3 :: Rule UnicodeDatabase
gb3 = ClusterState.mkSimpleRule "GB3" $
  \b1 b2 -> b1 == GraphemeClusterBreak_CR && b2 == GraphemeClusterBreak_LF

gb4 :: Rule UnicodeDatabase
gb4 = ClusterState.onPrevClusterChar $ \db state cs prevChar -> do
  let nextChar = state.input ! state.inputIdx
      b1 = ClusterState.graphemeBreakProperty db prevChar

  guard $ ClusterState.isControlCrLf b1

  pure $
    MkClusterState
      { lastRule = Just "GB4",
        clusters = MkClusters $ cs :|> ClusterChar prevChar :|> ClusterBreak :|> ClusterChar nextChar,
        input = state.input,
        inputIdx = state.inputIdx + 1
      }

gb5 :: Rule UnicodeDatabase
gb5 = ClusterState.onPrevClusterChar $ \db state cs prevChar -> do
  let nextChar = state.input ! state.inputIdx
      b2 = ClusterState.graphemeBreakProperty db nextChar

  guard $ ClusterState.isControlCrLf b2

  pure $
    MkClusterState
      { lastRule = Just "GB5",
        clusters = MkClusters $ cs :|> ClusterChar prevChar :|> ClusterBreak :|> ClusterChar nextChar,
        input = state.input,
        inputIdx = state.inputIdx + 1
      }

gb6 :: Rule UnicodeDatabase
gb6 = ClusterState.mkSimpleRule "GB6" $ \b1 b2 ->
  b1 == GraphemeClusterBreak_L
    && b2
      ∈ [ GraphemeClusterBreak_L,
          GraphemeClusterBreak_V,
          GraphemeClusterBreak_LV,
          GraphemeClusterBreak_LVT
        ]

gb7 :: Rule UnicodeDatabase
gb7 = ClusterState.mkSimpleRule "GB7" $ \b1 b2 ->
  b1 ∈ [GraphemeClusterBreak_LV, GraphemeClusterBreak_V]
    && b2 ∈ [GraphemeClusterBreak_V, GraphemeClusterBreak_T]

gb8 :: Rule UnicodeDatabase
gb8 = mkSimpleRule "GB8" $ \b1 b2 ->
  b1 ∈ [GraphemeClusterBreak_LVT, GraphemeClusterBreak_T]
    && b2 == GraphemeClusterBreak_T

gb9 :: Rule UnicodeDatabase
gb9 = mkSimpleRule "GB9" $ \_ b2 ->
  b2 ∈ [GraphemeClusterBreak_Extend, GraphemeClusterBreak_ZWJ]

gb9a :: Rule UnicodeDatabase
gb9a = ClusterState.onPrevClusterChar $ \db state cs prevChar -> do
  let nextChar = state.input ! state.inputIdx
      b2 = graphemeBreakProperty db nextChar

  -- NOTE: [GB9a/b extended grapheme]
  --
  -- The standard says:
  --
  --     "The GB9a and GB9b rules only apply to extended grapheme clusters"
  --
  -- which I _thought_ implied we should be checking that these chars
  -- are considered grapheme extend e.g.
  --
  --    guard $ isGraphemeExtend db (next|prev)Char
  --
  -- That causes tests to fail, however.
  guard $ b2 == GraphemeClusterBreak_SpacingMark

  pure $
    MkClusterState
      { lastRule = Just "GB9a",
        clusters = MkClusters $ cs :|> ClusterChar prevChar :|> ClusterChar nextChar,
        input = state.input,
        inputIdx = state.inputIdx + 1
      }

gb9b :: Rule UnicodeDatabase
gb9b = ClusterState.onPrevCluster $ \db state cs prev ->
  case prev of
    ClusterBreak -> Nothing
    ClusterChar prevChar -> do
      let nextChar = state.input ! state.inputIdx
          b1 = graphemeBreakProperty db prevChar

      -- See NOTE: [GB9a/b extended grapheme]
      guard $ b1 == GraphemeClusterBreak_Prepend

      pure $
        MkClusterState
          { lastRule = Just "GB9b",
            clusters = MkClusters $ cs :|> prev :|> ClusterChar nextChar,
            input = state.input,
            inputIdx = state.inputIdx + 1
          }

gb9c :: Rule UnicodeDatabase
gb9c = ClusterState.onPrevCluster $ \db state cs prev -> do
  let lookBack b i = do
        c <- state.input !? i

        if
          -- 1. Stop once we find another consonant
          | isIcbConsonant c -> pure b
          -- 2. Found linker: set flag, look for consonant
          | isIcbLinker c -> lookBack True (i - 1)
          -- 3. Found extend: fine, keep looking
          | isIcbExtend c -> lookBack b (i - 1)
          -- 4. O/w fail.
          | otherwise -> Nothing

      derived = db.unUnicodeDatabase.derivedCoreProperties
      isIcbConsonant c = HSet.member c derived.indicConjunctBreakConsonant
      isIcbExtend c = HSet.member c derived.indicConjunctBreakExtend
      isIcbLinker c = HSet.member c derived.indicConjunctBreakLinker

  -- Next char is a consonant. Need to look at previous input.
  let nextChar = state.input ! state.inputIdx
  guard $ isIcbConsonant nextChar

  -- Search for linker and previous consonant.
  matchesRule <- lookBack False (state.inputIdx - 1)
  guard matchesRule

  pure $
    MkClusterState
      { lastRule = Just "GB9c",
        clusters = MkClusters $ cs :|> prev :|> ClusterChar nextChar,
        input = state.input,
        inputIdx = state.inputIdx + 1
      }

gb11 :: Rule UnicodeDatabase
gb11 = ClusterState.onPrevCluster $ \db state cs prev -> do
  let lookBack i = do
        c <- state.input !? i

        if
          -- 1. Stop once we find another consonant
          | isExtendedPictograph c -> pure ()
          -- 2. Is extend: find, skip.
          | isExtend c -> lookBack (i - 1)
          -- 4. O/w fail.
          | otherwise -> Nothing

      extendedPictographs = db.unUnicodeDatabase.emojiData.extendedPictographic
      isExtendedPictograph c = HSet.member c extendedPictographs

      isExtend c = GraphemeClusterBreak_Extend == graphemeBreakProperty db c

  -- Next char is a consonant. Need to look at previous input.
  let nextChar = state.input ! state.inputIdx
  guard $ isExtendedPictograph nextChar

  -- previous char must be ZWJ
  assertChar state.input (state.inputIdx - 1) $ \c ->
    GraphemeClusterBreak_ZWJ == graphemeBreakProperty db c

  -- Search for extended pictograph.
  lookBack (state.inputIdx - 2)

  pure $
    MkClusterState
      { lastRule = Just "GB11",
        clusters = MkClusters $ cs :|> prev :|> ClusterChar nextChar,
        input = state.input,
        inputIdx = state.inputIdx + 1
      }

gb12_13 :: Rule UnicodeDatabase
gb12_13 = ClusterState.onPrevCluster $ \db state cs prev -> do
  let lookBack isOdd i = case state.input !? i of
        Nothing -> isOdd
        Just c ->
          if
            -- 1. Found RI. Add parity and continue
            | isRegionalIndicator c -> lookBack (isOdd `xor` True) (i - 1)
            -- 4. O/w return what we have found.
            | otherwise -> isOdd

      isRegionalIndicator c =
        GraphemeClusterBreak_Regional_Indicator == graphemeBreakProperty db c

  -- Next char is a Regional_Indicator. Need to look at previous input.
  let nextChar = state.input ! state.inputIdx
  guard $ isRegionalIndicator nextChar

  let isOdd = lookBack False (state.inputIdx - 1)
  guard isOdd

  pure $
    MkClusterState
      { lastRule = Just "GB12_13",
        clusters = MkClusters $ cs :|> prev :|> ClusterChar nextChar,
        input = state.input,
        inputIdx = state.inputIdx + 1
      }
  where
    xor True True = False
    xor False False = False
    xor _ _ = True
