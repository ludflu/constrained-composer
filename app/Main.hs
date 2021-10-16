{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
-- {-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Data.SBV
import Data.List
import Data.Maybe
import qualified Data.SBV.List as L
import Data.SBV.Tools.BoundedList

windows :: Int -> [a] -> [[a]]
windows n xs = let wnds = map (take n) (tails xs)
                in filter (\x -> (length x) == n) wnds

listToPair :: [x] -> Maybe(x,x)
listToPair (_:[]) = Nothing
listToPair [] = Nothing
listToPair (l:ls) = Just (l, head ls)

listsToPairs :: [[x]] -> [(x,x)]
listsToPairs ls = catMaybes $ map listToPair ls

makePairs :: [x] -> [(x,x)]
makePairs = listsToPairs . (windows 2)

absDiff :: SBV Integer -> SBV Integer -> SBV Integer
absDiff a b = abs (a-b)

diff :: SBV Integer -> SBV Integer -> SBV Integer
diff a b = a-b

fromSBool :: SBool -> Bool
fromSBool sFalse = False
fromSBool sTrue = True

onlyLeaps :: [(SBV Integer, SBV Integer)] -> [(SBV Integer, SBV Integer)]
onlyLeaps dps = filter (\d -> fromSBool ( abs (fst d) .> 1)) dps

--leaps rebound with a step in the opposite direction
leapsRebound :: SBV Integer -> SBV Integer -> SBool
leapsRebound a b = (signum a ./= signum b) .&& (abs b .==1)

--given a list of scale degrees, how many are steps (not leaps)
numSteps :: SList Integer -> SBV Integer
numSteps ss = let steps = bfilter 100 (\x -> (x .== 1)) ss
               in L.length steps

--given a list of scale degrees, how many are leaps, not steps
numLeaps :: SList Integer -> SBV Integer
numLeaps ss = let steps = bfilter 100 (\x -> (x .> 1)) ss
               in L.length steps

--given a list of scale degrees, how many are repeated notes
numRepeats :: SList Integer -> SBV Integer
numRepeats ss = let steps = bfilter 100 (\x -> (x .== 0)) ss
               in L.length steps


vnames :: [String]
vnames = let vs = take 16 ['a'..]
          in map (\v -> [v]) vs

mkSList :: [SBV Integer] -> SList Integer
mkSList xs = L.implode xs



--mphrase :: IO AllSatResult
mphrase :: IO SatResult
mphrase = sat $ do
    svars <- mapM sInteger vnames
    let isScaleDegree x = x .>= 1 .&& x .<= 7   --scale degrees
        vfirst = head svars
        vlast = last svars

        delta = map (uncurry diff) pairs
        deltaPairs = makePairs delta
        leaps = onlyLeaps deltaPairs

        pairs = makePairs svars
        adelta = mkSList $ map (uncurry absDiff) pairs
        stepCount = numSteps adelta
        leapCount = numLeaps adelta
        repeatCount = numRepeats adelta

    constrain $ sAll isScaleDegree svars
    solve [ stepCount .>= 14, --mostly steps
            leapCount .<= 2 , --some leaps allowed
            repeatCount .<= 1, --no more than 1 repeated note
            vfirst .== 1,  --begin on the tonic
            vlast .== 1,  --end on the tonic
            sAll (uncurry leapsRebound) leaps --all leaps should be followed by a step in the opposite direction
          ]


main :: IO ()
main = print =<< mphrase
