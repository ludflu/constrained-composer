{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedLists #-}

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

numSteps :: SList Integer -> SBV Integer
numSteps ss = let steps = bfilter 100 (\x -> (x .== 1)) ss
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
        _absDiff = uncurry absDiff
        pairs = makePairs svars
        steps = map _absDiff pairs
        stepCount = numSteps (mkSList steps)
    constrain $ sAll isScaleDegree svars
    constrain $ stepCount .>= 10
    constrain $ vfirst .== 5
    solve [ (abs vfirst - abs vlast) .== 0 ]


main :: IO ()
main = print =<< mphrase
