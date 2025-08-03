module Unicode.Grapheme.Internal.V15_1
  ( -- * Breakers
    breakGraphemeClusters,
    breakGraphemeClustersRules,
    breakGraphemeClustersStates,

    -- * Width
    clusterWidth,

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
import Data.Sequence (Seq (Empty))
import Data.Sequence qualified as Seq
import Data.Text (Text)
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
  ( ClusterOutput (ClusterChar),
    ClusterState (MkClusterState, clusters, input, inputIdx, lastRule),
    Clusters (MkClusters, unClusters),
    Rule (MkRule),
    RulesMatched,
    assertChar,
    graphemeBreakProperty,
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
import Unicode.Grapheme.Internal.Width qualified as Width

breakGraphemeClusters :: Text -> [Text]
breakGraphemeClusters =
  ClusterState.breakGraphemeClusters database rules

breakGraphemeClustersRules :: Text -> (RulesMatched, [Text])
breakGraphemeClustersRules =
  ClusterState.breakGraphemeClustersRules database rules

breakGraphemeClustersStates :: Text -> Seq ClusterState
breakGraphemeClustersStates =
  ClusterState.breakGraphemeClustersStates database rules

clusterWidth :: Text -> Int
clusterWidth = Width.clusterWidth database.unUnicodeDatabase

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

  case ClusterState.stateIndex state 0 of
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

  -- NOTE: This rule is called [0.3] in the test file.
  pure $ ClusterState.stateIncIndex state "GB2"

gb3 :: Rule UnicodeDatabase
gb3 = ClusterState.matchGCBsSimple "GB3" $
  \b1 b2 -> b1 == GraphemeClusterBreak_CR && b2 == GraphemeClusterBreak_LF

gb4 :: Rule UnicodeDatabase
gb4 = ClusterState.matchGCBsBreakSimple "GB4" $
  \b1 _ -> ClusterState.isControlCrLf b1

gb5 :: Rule UnicodeDatabase
gb5 = ClusterState.matchGCBsBreakSimple "GB5" $
  \_ b2 -> ClusterState.isControlCrLf b2

gb6 :: Rule UnicodeDatabase
gb6 = ClusterState.matchGCBsSimple "GB6" $ \b1 b2 ->
  b1 == GraphemeClusterBreak_L
    && b2
      ∈ [ GraphemeClusterBreak_L,
          GraphemeClusterBreak_V,
          GraphemeClusterBreak_LV,
          GraphemeClusterBreak_LVT
        ]

gb7 :: Rule UnicodeDatabase
gb7 = ClusterState.matchGCBsSimple "GB7" $ \b1 b2 ->
  b1 ∈ [GraphemeClusterBreak_LV, GraphemeClusterBreak_V]
    && b2 ∈ [GraphemeClusterBreak_V, GraphemeClusterBreak_T]

gb8 :: Rule UnicodeDatabase
gb8 = ClusterState.matchGCBsSimple "GB8" $ \b1 b2 ->
  b1 ∈ [GraphemeClusterBreak_LVT, GraphemeClusterBreak_T]
    && b2 == GraphemeClusterBreak_T

gb9 :: Rule UnicodeDatabase
gb9 = ClusterState.matchGCBsSimple "GB9" $ \_ b2 ->
  b2 ∈ [GraphemeClusterBreak_Extend, GraphemeClusterBreak_ZWJ]

gb9a :: Rule UnicodeDatabase
gb9a = ClusterState.matchGCBsSimple "GB9a" $ \_ b2 ->
  b2 == GraphemeClusterBreak_SpacingMark

gb9b :: Rule UnicodeDatabase
gb9b = ClusterState.matchGCBsSimple "GB9b" $ \b1 _ ->
  b1 == GraphemeClusterBreak_Prepend

gb9c :: Rule UnicodeDatabase
gb9c = ClusterState.onPrevCluster_ $ \db state -> do
  let lookBack !b !i = do
        c <- ClusterState.stateIndex state i

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
  let nextChar = ClusterState.stateUnsafeNextChar state
  guard $ isIcbConsonant nextChar

  -- Search for linker and previous consonant.
  matchesRule <- lookBack False (state.inputIdx - 1)
  guard matchesRule

  pure $ ClusterState.stateAppendChar state "GB9c" nextChar

gb11 :: Rule UnicodeDatabase
gb11 = ClusterState.onPrevCluster_ $ \db state -> do
  let lookBack !i = do
        c <- ClusterState.stateIndex state i

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
  let nextChar = ClusterState.stateUnsafeNextChar state
  guard $ isExtendedPictograph nextChar

  -- previous char must be ZWJ
  assertChar state.input (state.inputIdx - 1) $ \c ->
    GraphemeClusterBreak_ZWJ == graphemeBreakProperty db c

  -- Search for extended pictograph.
  lookBack (state.inputIdx - 2)

  pure $ ClusterState.stateAppendChar state "GB11" nextChar

gb12_13 :: Rule UnicodeDatabase
gb12_13 = ClusterState.onPrevCluster_ $ \db state -> do
  let lookBack !isOdd !i = case ClusterState.stateIndex state i of
        Nothing -> isOdd
        Just c ->
          if isRegionalIndicator c
            -- 1. Found RI. Add parity and continue
            then lookBack (isOdd `xor` True) (i - 1)
            -- 2. O/w return what we have found.
            else isOdd

      isRegionalIndicator c =
        GraphemeClusterBreak_Regional_Indicator == graphemeBreakProperty db c

  -- Next char is a Regional_Indicator. Need to look at previous input.
  let nextChar = ClusterState.stateUnsafeNextChar state
  guard $ isRegionalIndicator nextChar

  let isOdd = lookBack False (state.inputIdx - 1)
  guard isOdd

  pure $ ClusterState.stateAppendChar state "GB12_13" nextChar
  where
    xor True True = False
    xor False False = False
    xor _ _ = True
