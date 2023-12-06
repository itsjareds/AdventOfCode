#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle pkgs.split ])" 

{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Read
import Data.Text (Text)
import Data.List
import Data.List.Split
import qualified Data.Text as T

data Map = Map Range Int deriving (Show)
data Range = Range Int Int deriving (Show)

main = do
  content <- readFile "input2"

  let file = T.breakOn "\n" . T.pack $ content
  let seeds = parseSeeds . fst $ file
  print seeds

  let maps = map parseMap . T.splitOn "\n\n" . T.strip . snd $ file
  print maps

  print "Part 1"
  print . map (findLocation maps) $ seeds
  print . minimum . map (findLocation maps) $ seeds

  --print "Part 2"
  --let seeds' = genRange seeds
  --print seeds'
  --print . minimum . map (findLocation maps) $ seeds'


--optimizeMaps :: Range -> [[Map]] -> [Map]
--optimizeMaps r ms
--
--optimize :: Range -> [Map] -> Mapdest_start + i
--optimize r ms = foldl' (intersect) [] ms
--
--intersect :: Map -> Range -> Map
--intersect m r = 

findLocation :: [[Map]] -> Int -> Int
findLocation ms n = foldl' (traverseMaps) n ms

traverseMaps :: Int -> [Map] -> Int
traverseMaps n [] = n
traverseMaps n ((Map (Range start end) offset):ms) =
  let index = rangeIndex (Range start end) n
      dest = start + offset
  in case index of
    Just i -> dest + i
    Nothing -> traverseMaps n ms

rangeIndex :: Range -> Int -> Maybe Int
rangeIndex (Range start end) n
  | start <= n && n <= end = Just (n - start)
  | otherwise = Nothing

parseSeeds :: Text -> [Int]
parseSeeds txt = map num . tail . T.words $ txt

genRange :: [Int] -> [Int]
genRange xs = concat [ [a..a+b-1] | (a:b:_) <- divvy 2 2 xs ]

parseMap :: Text -> [Map]
parseMap txt =
  let content = T.lines . T.strip $ txt
      key = head . T.words . head $ content
      values = map (map num . T.words) . tail $ content
  in map parseMapLine values

parseMapLine :: [Int] -> Map
parseMapLine [a,b,c] = Map (Range b (b+c-1)) (a-b)



-- Cast text to int
num :: Text -> Int
num = either (error . show) fst
    . signed decimal

