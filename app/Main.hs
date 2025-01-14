{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- {-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Data.List (tails)
import Data.Maybe (mapMaybe)
import Data.SBV
import qualified Data.SBV.List as L
import Data.SBV.Tools.BoundedList (bfilter)

type SMInt = SBV Integer

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
fromSBool sTrue = True

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
  let vs = take 16 ['a' ..]
   in map (\v -> [v]) vs

mphrase :: IO AllSatResult
mphrase = allSatWith defaultSMTCfg {allSatMaxModelCount = Just 10} $ do
  svars <- mapM sInteger vnames
  let isScaleDegree x = x .>= 1 .&& x .<= 7 --scale degrees
      vfirst = head svars
      vlast = last svars

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
      vfirst .== 1, --begin on the tonic
      vlast .== 1, --end on the tonic
      sAll (.>= 1) bigramDiffs, --no repeated bigrams
      sAll (uncurry leapsRebound) leaps --all leaps should be followed by a step in the opposite direction
    ]

main :: IO ()
main = print =<< mphrase
