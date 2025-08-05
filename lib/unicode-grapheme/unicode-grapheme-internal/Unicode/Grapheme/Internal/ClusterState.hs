{-# LANGUAGE ViewPatterns #-}

-- | Functionality for applying rules to the cluster state.
module Unicode.Grapheme.Internal.ClusterState
  ( -- * Breaking clusters
    Rule (..),
    breakGraphemeClusters,

    -- ** Rules
    RulesMatched (..),
    breakGraphemeClustersRules,

    -- * State

    -- ** Cluster State
    ClusterState (..),
    breakGraphemeClustersStates,

    -- *** View
    stateUnsafeNextChar,
    stateNextChar,
    stateIndex,
    stateUnsafeIndex,

    -- *** Modify
    stateAppendChar,
    stateAppendCluster,
    stateIncIndex,

    -- *** Display
    displayClusterStates,

    -- ** Clusters
    Clusters (.., CEmpty, (:*>), (:<*)),
    ClusterOutput (..),

    -- * Creating Rules
    matchGCBs,
    matchGCBsSimple,
    matchGCBsBreak,
    matchGCBsBreakSimple,
    onPrevCluster,
    onPrevCluster_,
    onPrevClusterChar,
    onPrevClusterChar_,

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
import Unicode.Grapheme.Internal.DB.GraphemeClusterBreak
  ( GraphemeClusterBreak
      ( GraphemeClusterBreak_Any,
        GraphemeClusterBreak_CR,
        GraphemeClusterBreak_Control,
        GraphemeClusterBreak_LF
      ),
  )
import Unicode.Grapheme.Internal.DB.Parsing qualified as Parsing
import Unicode.Grapheme.Internal.DB.Properties
  ( GraphemeBreakProperties (unGraphemeBreakProperties),
    Properties (graphemeBreakProperties),
  )

-- https://www.unicode.org/reports/tr29/#Grapheme_Cluster_Break_Property_Values

-------------------------------------------------------------------------------
--                             Breaking Clusters                             --
-------------------------------------------------------------------------------

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

-- | Rules that were used during cluster breaking.
newtype RulesMatched = MkRulesMatched {unRulesMatched :: Seq Text}
  deriving stock (Eq, Show)
  deriving newtype (Monoid, Semigroup)

-------------------------------------------------------------------------------
--                               Cluster State                               --
-------------------------------------------------------------------------------

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

-- | Initial state.
mkInitState :: [Char] -> ClusterState
mkInitState cs =
  MkClusterState
    { lastRule = Nothing,
      clusters = mempty,
      input = V.fromList cs,
      inputIdx = 0
    }

-- | Displays cluster states.
displayClusterStates :: Seq ClusterState -> Text
displayClusterStates Empty = "<no states>"
displayClusterStates allStates@(s0 :<| _) =
  mconcat
    [ "Input:",
      mspc,
      inputStr,
      "\n",
      statesStrs
    ]
  where
    mspc =
      if T.null inputStr
        then ""
        else " "
    inputStr =
      T.intercalate " "
        . fmap (T.pack . Parsing.charToHexStringPad4)
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
      ":",
      mspc,
      clustersTxt
    ]
  where
    mspc =
      if T.null clustersTxt
        then ""
        else " "

    clustersTxt = displayClusters state.clusters

displayMRule :: Maybe Text -> Text
displayMRule (Just r) = r
displayMRule Nothing = "<no rule>"

stateAppendChar :: ClusterState -> Text -> Char -> ClusterState
stateAppendChar state name nextChar =
  MkClusterState
    { lastRule = Just name,
      clusters = state.clusters :*> ClusterChar nextChar,
      input = state.input,
      inputIdx = state.inputIdx + 1
    }

stateAppendCluster :: ClusterState -> Text -> Char -> ClusterState
stateAppendCluster state name nextChar =
  MkClusterState
    { lastRule = Just name,
      clusters = state.clusters :*> ClusterBreak :*> ClusterChar nextChar,
      input = state.input,
      inputIdx = state.inputIdx + 1
    }

stateIncIndex :: ClusterState -> Text -> ClusterState
stateIncIndex state name =
  MkClusterState
    { lastRule = Just name,
      clusters = state.clusters,
      input = state.input,
      inputIdx = state.inputIdx + 1
    }

stateUnsafeIndex :: ClusterState -> Int -> Char
stateUnsafeIndex state = (state.input !)

stateIndex :: ClusterState -> Int -> Maybe Char
stateIndex state = (state.input !?)

stateUnsafeNextChar :: ClusterState -> Char
stateUnsafeNextChar state = stateUnsafeIndex state state.inputIdx

stateNextChar :: ClusterState -> Maybe Char
stateNextChar state = stateIndex state state.inputIdx

-------------------------------------------------------------------------------
--                                 Clusters                                  --
-------------------------------------------------------------------------------

-- | Cluster output.
newtype Clusters = MkClusters {unClusters :: Seq ClusterOutput}
  deriving stock (Eq, Show)
  deriving newtype (Monoid, Semigroup)

pattern CEmpty :: Clusters
pattern CEmpty <- MkClusters Empty
  where
    CEmpty = MkClusters Empty

pattern (:*>) :: Clusters -> ClusterOutput -> Clusters
pattern cs :*> c <- (\(MkClusters (cs :|> c)) -> (MkClusters cs, c) -> (cs, c))
  where
    MkClusters cs :*> c = MkClusters (cs :|> c)

{-# COMPLETE (:*>), CEmpty #-}

infixl 5 :*>

pattern (:<*) :: ClusterOutput -> Clusters -> Clusters
pattern c :<* cs <- (\(MkClusters (c :<| cs)) -> (c, MkClusters cs) -> (c, cs))
  where
    c :<* MkClusters cs = MkClusters (c :<| cs)

{-# COMPLETE (:<*), CEmpty #-}

infixr 5 :<*

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
        . fmap (T.pack . Parsing.charToHexStringPad4)

-- | Transforms cluster state into output 'Text' clusters.
unpack :: Clusters -> [Text]
unpack =
  F.toList
    . fmap (T.pack . F.toList)
    . toCodePoints

toCodePoints :: Clusters -> Seq (Seq Char)
toCodePoints = F.foldl' toChar Empty . (.unClusters)
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
gb999 state = stateAppendCluster state "GB999" nextChar
  where
    nextChar = stateUnsafeNextChar state

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
  (db -> ClusterState -> ClusterOutput -> Maybe ClusterState) ->
  Rule db
onPrevCluster f = MkRule $ \db state -> case state.clusters of
  _ :*> c -> f db state c
  _ -> Nothing

onPrevCluster_ ::
  (db -> ClusterState -> Maybe ClusterState) ->
  Rule db
onPrevCluster_ f = onPrevCluster $ \db state _ -> f db state

-- | Runs the rule when the previous cluster element is a char (i.e. not a
-- break).
onPrevClusterChar ::
  (db -> ClusterState -> Char -> Maybe ClusterState) ->
  Rule db
onPrevClusterChar f = MkRule $ \db state -> case state.clusters of
  _ :*> ClusterChar c -> f db state c
  _ -> Nothing

-- | Runs the rule when the previous cluster element is a char (i.e. not a
-- break).
onPrevClusterChar_ ::
  (db -> ClusterState -> Maybe ClusterState) ->
  Rule db
onPrevClusterChar_ f = onPrevClusterChar $ \db state _ -> f db state

-- | Helper for running the rule on the previous and next cluster breaks.
matchGCBs ::
  (HasField "unUnicodeDatabase" db Properties) =>
  Text ->
  (db -> ClusterState -> GraphemeClusterBreak -> GraphemeClusterBreak -> Bool) ->
  Rule db
matchGCBs name matchGcbs = onPrevClusterChar $ \db state prevChar -> do
  let nextChar = stateUnsafeNextChar state
      b1 = graphemeBreakProperty db prevChar
      b2 = graphemeBreakProperty db nextChar

  guard $ matchGcbs db state b1 b2

  pure $ stateAppendChar state name nextChar

-- | Helper for running the rule on the previous and next cluster breaks.
matchGCBsBreak ::
  (HasField "unUnicodeDatabase" db Properties) =>
  Text ->
  (db -> ClusterState -> GraphemeClusterBreak -> GraphemeClusterBreak -> Bool) ->
  Rule db
matchGCBsBreak name matchGcbs = onPrevClusterChar $ \db state prevChar -> do
  let nextChar = stateUnsafeNextChar state
      b1 = graphemeBreakProperty db prevChar
      b2 = graphemeBreakProperty db nextChar

  guard $ matchGcbs db state b1 b2

  pure $ stateAppendCluster state name nextChar

-- | Simpler version of 'matchGCBs' that does not require the database or
-- state.
matchGCBsSimple ::
  (HasField "unUnicodeDatabase" db Properties) =>
  Text ->
  (GraphemeClusterBreak -> GraphemeClusterBreak -> Bool) ->
  Rule db
matchGCBsSimple name matchGcbs = matchGCBs name (\_ _ -> matchGcbs)

-- | Simpler version of 'matchGCBsBreak' that does not require the database or
-- state.
matchGCBsBreakSimple ::
  (HasField "unUnicodeDatabase" db Properties) =>
  Text ->
  (GraphemeClusterBreak -> GraphemeClusterBreak -> Bool) ->
  Rule db
matchGCBsBreakSimple name matchGcbs = matchGCBsBreak name (\_ _ -> matchGcbs)

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
