-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou
--
-- In this variant of Bogobogosort, we sort the last (n-1) elements recursively
-- rather than the first (n-1).
--
-- For more information, see
-- https://www.dangermouse.net/esoteric/bogobogosort.html
--
-- The procedure is:
--
-- 1. Sort the last n-1 elements of the copy using bogobogosort.
-- 2. Check to see if the 1st element of the sorted copy is <= than the
--    first element of the last n-1 elements. If so, the copy is now sorted,
--    else randomise the order of the elements of the copy and go to step 1.
-- 3. Repeat.

import System.Random
import Data.Array.IO
import Control.Monad

bogobogosort :: (Ord a) => [a] -> IO [a]
bogobogosort []     = return []  -- base case for empty
bogobogosort [x]    = return [x] -- base case for singleton
bogobogosort (x:xs) = bogobogosort xs >>= step x -- step 1: sort last n-1 elems
    where
        step x (y:ys) -- step 2
            | x <= y    = return (x:y:ys)                 -- in order, done
            | otherwise = shuffle (x:xs) >>= bogobogosort -- shuffle again

-- Performs n times of bogobogosort on a random shuffle of xs
testbbs :: (Ord a, Show a) => Int -> [a] -> IO ()
testbbs n xs = let rs = shuffle xs
               in  foldl s (putStr "") [ rs >>= bogobogosort | i <- [1..n] ]
                    where
                        s n x = n >> x >>= \x -> putStrLn (show x)

-- Snippets copied from elsewhere --

-- https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
