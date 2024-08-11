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
  counterpoint <- mapM sInteger vnames

  let isScaleDegree x = x .>= 1 .&& x .<= 7 --scale degrees  
      vfirst = head counterpoint
      vlast = last counterpoint
      checkConsonance indx = isConsonantInterval (counterpoint !! indx) (cantusFirmus !! indx)

      consosnanceCheck = map checkConsonance [0 .. 14]

      counterpointBigrams = makePairs counterpoint
      counterPointMotion = map (uncurry diff) counterpointBigrams

      givenBigrams = makePairs cantusFirmus
      givenMotion = map (uncurry diff) givenBigrams


      deltaPairs = makePairs counterPointMotion
      leaps = onlyLeaps deltaPairs

      bigramPairs = makePairs counterpointBigrams
      bigramDiffs = map (uncurry pairDiff) bigramPairs

      adelta = L.implode $ map (uncurry absDiff) counterpointBigrams
      stepCount = numSteps adelta
      leapCount = numLeaps adelta
      repeatCount = numRepeats adelta

  constrain $ sAll isScaleDegree counterpoint
  let s = [ stepCount .>= 12, --mostly steps
        leapCount .<= 4, --some leaps allowed
        repeatCount .== 0, --no repeated notes
        vfirst .== vlast, --begin and end on the tonic
        sAll (.>= 1) bigramDiffs, --no repeated bigrams
        sAll (uncurry leapsRebound) leaps --all leaps should be followed by a step in the opposite direction
          ] ++ consosnanceCheck
   in solve s

main :: IO ()
main = print =<< mphrase
