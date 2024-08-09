{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- {-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Data.List (tails)
import Data.Maybe (mapMaybe)
import Data.SBV
    ( allSatWith,
      sInteger,
      solve,
      (.&&),
      sAll,
      defaultSMTCfg,
      SBV,
      SBool,
      SList,
      sFalse,
      sTrue,
      sMod,
      SInteger,
      constrain,
      (.||),
      EqSymbolic((.==), (./=)),
      OrdSymbolic((.>=), (.>), (.<=)),
      SMTConfig(allSatMaxModelCount),
      AllSatResult )
import qualified Data.SBV.List as L
import Data.SBV.Tools.BoundedList (bfilter)

type SMInt = SBV Integer


type Counterpoint = [SInteger]

cantusFirmus :: Counterpoint = [60,62,64,60,67,65,64,69,67,60,62,65,64,62,60]

interval :: SInteger -> SInteger -> SMInt
interval given counter = let given' = given `sMod` 12
                             counter' = counter `sMod` 12
                             i =  given' - counter'
                          in i `sMod` 12

isConsonant :: SMInt -> SBool
isConsonant i = i .== 0 .|| i .== 3 .|| i .== 4 .|| i .== 7 .|| i .== 8 .|| i .== 9 .|| i .== 12

isConsonantInterval :: SInteger -> SInteger -> SBool
isConsonantInterval given counter = isConsonant (interval given counter)

windows :: Int -> [a] -> [[a]]
windows n xs =
  let wnds = map (take n) (tails xs)
   in filter (\x -> length x == n) wnds

listToPair :: [x] -> Maybe (x, x)
listToPair [_] = Nothing
listToPair [] = Nothing
listToPair (l : ls) = Just (l, head ls)

listsToPairs :: [[x]] -> [(x, x)]
listsToPairs = mapMaybe listToPair

makePairs :: [x] -> [(x, x)]
makePairs = listsToPairs . windows 2

absDiff :: SMInt -> SMInt -> SMInt
absDiff a b = abs (a - b)

diff :: SMInt -> SMInt -> SMInt
diff a b = a - b

pairDiff :: (SMInt, SMInt) -> (SMInt, SMInt) -> SMInt
pairDiff (a, b) (x, y) = (absDiff a x) + (absDiff b y)

fromSBool :: SBool -> Bool
fromSBool sFalse = False
-- fromSBool sTrue = True -- compiler says this is redundant

toSBool :: Bool -> SBool
toSBool False = sFalse
toSBool True = sTrue

onlyLeaps :: [(SMInt, SMInt)] -> [(SMInt, SMInt)]
onlyLeaps = filter (\d -> fromSBool (abs (fst d) .> 1))

--leaps rebound with a step in the opposite direction
leapsRebound :: SMInt -> SMInt -> SBool
leapsRebound a b = (signum a ./= signum b) .&& (abs b .== 1)

--given a list of scale degrees, how many are steps (not leaps)
numSteps :: SList Integer -> SMInt
numSteps ss =
  let steps = bfilter 100 (.== 1) ss
   in L.length steps

--given a list of scale degrees, how many are leaps, not steps
numLeaps :: SList Integer -> SMInt
numLeaps ss =
  let steps = bfilter 100 (.> 1) ss
   in L.length steps

--given a list of scale degrees, how many are repeated notes
numRepeats :: SList Integer -> SMInt
numRepeats ss =
  let steps = bfilter 100 (.== 0) ss
   in L.length steps

vnames :: [String]
vnames =
  let vs = take 15 ['a' ..]
   in map (\v -> [v]) vs

mphrase :: IO AllSatResult
mphrase = allSatWith defaultSMTCfg {allSatMaxModelCount = Just 1} $ do
  svars <- mapM sInteger vnames
  let isScaleDegree x = x .>= 1 .&& x .<= 7 --scale degrees

  
      vfirst = head svars
      vlast = last svars
      checkConsonance indx = isConsonantInterval (svars !! indx) (cantusFirmus !! indx)

      delta = map (uncurry diff) pairs
      deltaPairs = makePairs delta
      leaps = onlyLeaps deltaPairs

      pairs = makePairs svars
      bigramPairs = makePairs pairs
      bigramDiffs = map (uncurry pairDiff) bigramPairs

      adelta = L.implode $ map (uncurry absDiff) pairs
      stepCount = numSteps adelta
      leapCount = numLeaps adelta
      repeatCount = numRepeats adelta

  constrain $ sAll isScaleDegree svars
  solve
    [ stepCount .>= 12, --mostly steps
      leapCount .<= 4, --some leaps allowed
      repeatCount .== 0, --no repeated notes
      vfirst .== vlast, --begin and end on the tonic
      checkConsonance 0, --consonance with cantus firmus
      checkConsonance 1, --consonance with cantus firmus
      checkConsonance 2, --consonance with cantus firmus
      checkConsonance 3, --consonance with cantus firmus
      checkConsonance 4, --consonance with cantus firmus
      checkConsonance 5, --consonance with cantus firmus
      checkConsonance 6, --consonance with cantus firmus
      checkConsonance 7, --consonance with cantus firmus
      checkConsonance 8, --consonance with cantus firmus
      checkConsonance 9, --consonance with cantus firmus
      checkConsonance 10, --consonance with cantus firmus
      checkConsonance 11, --consonance with cantus firmus
      checkConsonance 12, --consonance with cantus firmus
      checkConsonance 13, --consonance with cantus firmus
      checkConsonance 14, --consonance with cantus firm 
      sAll (.>= 1) bigramDiffs, --no repeated bigrams
      sAll (uncurry leapsRebound) leaps --all leaps should be followed by a step in the opposite direction
    ]

main :: IO ()
main = print =<< mphrase
