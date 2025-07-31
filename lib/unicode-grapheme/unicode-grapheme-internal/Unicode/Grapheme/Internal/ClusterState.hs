-- | Functionality for applying rules to the cluster state.
module Unicode.Grapheme.Internal.ClusterState
  ( -- * Breaking clusters
    Rule (..),
    breakGraphemeClusters,

    -- ** Rules
    RulesMatched (..),
    breakGraphemeClustersRules,

    -- ** Intermediate states
    ClusterState (..),
    Clusters (..),
    ClusterOutput (..),
    displayClusterStates,
    breakGraphemeClustersStates,

    -- * Creating Rules
    mkSimpleRule,
    onPrevCluster,
    onPrevClusterChar,

    -- * Misc
    (∈),
    assertChar,
    graphemeBreakProperty,
    isControlCrLf,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Data.Foldable qualified as F
import Data.HashMap.Strict qualified as HMap
import Data.HashSet qualified as HSet
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (Empty, (:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Strict (Vector, (!), (!?))
import Data.Vector.Strict qualified as V
import GHC.Records (HasField)
import Unicode.Grapheme.Common.DB.GraphemeClusterBreak
  ( GraphemeClusterBreak
      ( GraphemeClusterBreak_Any,
        GraphemeClusterBreak_CR,
        GraphemeClusterBreak_Control,
        GraphemeClusterBreak_LF
      ),
  )
import Unicode.Grapheme.Common.DB.Parsing qualified as Parsing
import Unicode.Grapheme.Internal.DB.Properties
  ( GraphemeBreakProperties (unGraphemeBreakProperties),
    Properties (graphemeBreakProperties),
  )

-- https://www.unicode.org/reports/tr29/#Grapheme_Cluster_Break_Property_Values

-- | Breaks the text into grapheme clusters per database and rules.
breakGraphemeClusters :: db -> [Rule db] -> Text -> [Text]
breakGraphemeClusters db rules =
  unpack
    . runState db rules
    . mkInitState
    . T.unpack

-- | Like 'breakGraphemeClusters', but returns the matched rules.
breakGraphemeClustersRules :: db -> [Rule db] -> Text -> (RulesMatched, [Text])
breakGraphemeClustersRules db rules txt =
  case breakGraphemeClustersStates db rules txt of
    Empty -> (MkRulesMatched Empty, [])
    allStates@(_ :|> finalState) ->
      let allRules =
            fmap displayMRule
              . dropFirstNothing
              . fmap (.lastRule)
              $ allStates
       in ( MkRulesMatched allRules,
            unpack finalState.clusters
          )
  where
    dropFirstNothing (Nothing :<| xs) = xs
    dropFirstNothing xs = xs

-- | Like 'breakGraphemeClusters', but returns the intermediate states.
breakGraphemeClustersStates :: db -> [Rule db] -> Text -> Seq ClusterState
breakGraphemeClustersStates db rules =
  runStates db rules
    . mkInitState
    . T.unpack

-- | Initial state.
mkInitState :: [Char] -> ClusterState
mkInitState cs =
  MkClusterState
    { lastRule = Nothing,
      clusters = mempty,
      input = V.fromList cs,
      inputIdx = 0
    }

-- | Transforms cluster state into output 'Text' clusters.
unpack :: Clusters -> [Text]
unpack =
  F.toList
    . fmap (T.pack . F.toList)
    . toCodePoints

toCodePoints :: Clusters -> Seq (Seq Char)
toCodePoints = F.foldl' toChar Empty . unClusters
  where
    toChar :: Seq (Seq Char) -> ClusterOutput -> Seq (Seq Char)
    toChar Empty ClusterBreak = Empty
    toChar Empty (ClusterChar c) = Seq.singleton (Seq.singleton c)
    toChar (clusters :|> cluster) ClusterBreak = (clusters :|> cluster) :|> Empty
    toChar (clusters :|> cluster) (ClusterChar c) = clusters :|> (cluster :|> c)

-- | Cluster output element.
data ClusterOutput
  = -- | Normal code point.
    ClusterChar Char
  | -- | Break.
    ClusterBreak
  deriving stock (Eq, Show)

-- | Cluster output.
newtype Clusters = MkClusters {unClusters :: Seq ClusterOutput}
  deriving stock (Eq, Show)
  deriving newtype (Monoid, Semigroup)

displayClusters :: Clusters -> Text
displayClusters = render . toCodePoints
  where
    render :: Seq (Seq Char) -> Text
    render =
      mconcat
        . F.toList
        . Seq.intersperse " ÷ "
        . fmap renderCluster

    renderCluster :: Seq Char -> Text
    renderCluster =
      mconcat
        . F.toList
        . Seq.intersperse " × "
        . fmap (T.pack . Parsing.charToHexStringPadN 4)

-- | Rules that were used during cluster breaking.
newtype RulesMatched = MkRulesMatched {unRulesMatched :: Seq Text}
  deriving stock (Eq, Show)
  deriving newtype (Monoid, Semigroup)

-- | Cluster state.
data ClusterState = MkClusterState
  { -- | The last rule that was used.
    lastRule :: Maybe Text,
    -- | Current cluster output.
    clusters :: Clusters,
    -- | Cluster input.
    input :: Vector Char,
    -- | Cluster input index.
    inputIdx :: Int
  }
  deriving stock (Eq, Show)

-- | Displays cluster states.
displayClusterStates :: Seq ClusterState -> Text
displayClusterStates Empty = "<no states>"
displayClusterStates allStates@(s0 :<| _) =
  mconcat
    [ "Input: ",
      inputStr,
      "\n",
      statesStrs
    ]
  where
    inputStr =
      T.intercalate " "
        . fmap (T.pack . Parsing.charToHexStringPadN 4)
        . F.toList
        $ s0.input

    statesStrs = T.intercalate "\n" . F.toList $ displayClusterState <$> allStates

displayClusterState :: ClusterState -> Text
displayClusterState state =
  mconcat
    [ "Step ",
      T.pack $ show state.inputIdx,
      ". ",
      displayMRule state.lastRule,
      ": ",
      clustersTxt
    ]
  where
    clustersTxt = displayClusters state.clusters

displayMRule :: Maybe Text -> Text
displayMRule (Just r) = r
displayMRule Nothing = "<no rule>"

-------------------------------------------------------------------------------
--                               Runinng State                               --
-------------------------------------------------------------------------------

-- NOTE: runState and runStates allow the index to match the length, before
-- giving up. Normally we'd stop at length - 1, but we go past the end
-- so we can hit the "end text rule", known as GB2 / 0.3. This is done so that
-- the rules nicely match up with the test files.
--
-- The correctness depends on GB2 actually being run, so it is very important
-- this rule is not left out!

-- | Runs the state against the rules, returning the final clusters.
runState :: db -> [Rule db] -> ClusterState -> Clusters
runState db rules = go
  where
    go state
      | state.inputIdx > V.length state.input = state.clusters
      | otherwise = go $ applyRules db rules state

-- | Runes the state against the rules, returning all state transitions.
runStates :: db -> [Rule db] -> ClusterState -> Seq ClusterState
runStates db rules = go
  where
    go s1
      | s1.inputIdx > V.length s1.input = Seq.singleton s1
      | otherwise = s1 :<| go (applyRules db rules s1)

applyRules ::
  db ->
  [Rule db] ->
  ClusterState ->
  ClusterState
applyRules db rules state = fromMaybe (gb999 state) rulesResult
  where
    rulesResult = (F.fold rules).unRule db state

gb999 :: ClusterState -> ClusterState
gb999 state =
  MkClusterState
    { lastRule = Just "GB999",
      clusters =
        MkClusters $
          state.clusters.unClusters :|> ClusterBreak :|> ClusterChar nextChar,
      input = state.input,
      inputIdx = state.inputIdx + 1
    }
  where
    nextChar = state.input ! state.inputIdx

-------------------------------------------------------------------------------
--                                  Rules                                    --
-------------------------------------------------------------------------------

-- | Grapheme cluster rule. Takes the database and state as input.
newtype Rule db = MkRule {unRule :: db -> ClusterState -> Maybe ClusterState}

instance Semigroup (Rule db) where
  MkRule r1 <> r2 = MkRule $ \db state ->
    r1 db state <|> r2.unRule db state

instance Monoid (Rule db) where
  mempty = MkRule $ \_ _ -> Nothing

-- | Runs the rule when there is a least one previous cluster element
-- (i.e. not empty).
onPrevCluster ::
  (db -> ClusterState -> Seq ClusterOutput -> ClusterOutput -> Maybe ClusterState) ->
  Rule db
onPrevCluster f = MkRule $ \db state -> case state.clusters.unClusters of
  Empty -> Nothing
  cs :|> c -> f db state cs c

-- | Runs the rule when the previous cluster element is a char (i.e. not a
-- break).
onPrevClusterChar ::
  (db -> ClusterState -> Seq ClusterOutput -> Char -> Maybe ClusterState) ->
  Rule db
onPrevClusterChar f = MkRule $ \db state -> case state.clusters.unClusters of
  Empty -> Nothing
  _ :|> ClusterBreak -> Nothing
  cs :|> ClusterChar c -> f db state cs c

-- | Makes a simple rule that only requires the previous and next char's
-- 'GraphemeClusterBreak' type.
mkSimpleRule ::
  (HasField "unUnicodeDatabase" db Properties) =>
  Text ->
  (GraphemeClusterBreak -> GraphemeClusterBreak -> Bool) ->
  Rule db
mkSimpleRule name matchGcbs = onPrevClusterChar $ \db state cs prevChar -> do
  let nextChar = state.input ! state.inputIdx
      b1 = graphemeBreakProperty db prevChar
      b2 = graphemeBreakProperty db nextChar

  guard $ matchGcbs b1 b2

  pure $
    MkClusterState
      { lastRule = Just name,
        clusters = MkClusters $ cs :|> ClusterChar prevChar :|> ClusterChar nextChar,
        input = state.input,
        inputIdx = state.inputIdx + 1
      }

(∈) :: (Hashable a) => a -> [a] -> Bool
x ∈ xs = x `HSet.member` HSet.fromList xs

assertChar :: Vector Char -> Int -> (Char -> Bool) -> Maybe ()
assertChar v i p = do
  r <- v !? i
  guard $ p r

-------------------------------------------------------------------------------
--                              Break Property                               --
-------------------------------------------------------------------------------

graphemeBreakProperty ::
  (HasField "unUnicodeDatabase" db Properties) =>
  db ->
  Char ->
  GraphemeClusterBreak
graphemeBreakProperty db c = HMap.lookupDefault GraphemeClusterBreak_Any c m
  where
    m = db.unUnicodeDatabase.graphemeBreakProperties.unGraphemeBreakProperties

isControlCrLf :: GraphemeClusterBreak -> Bool
isControlCrLf b =
  b
    ∈ [ GraphemeClusterBreak_Control,
        GraphemeClusterBreak_CR,
        GraphemeClusterBreak_LF
      ]
