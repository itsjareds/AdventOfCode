#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle pkgs.split ])" 

{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Read
import Data.Text (Text)
import Data.List
import Data.List.Split
import qualified Data.Text as T

data Map = Map Range Range deriving (Show)
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


findLocation :: [[Map]] -> Int -> Int
findLocation ms n = foldl' (traverseMaps) n ms

traverseMaps :: Int -> [Map] -> Int
traverseMaps n [] = n
traverseMaps n ((Map (Range dest_start _) source):ms) =
  let index = rangeIndex source n
  in case index of
    Just i -> dest_start + i
    Nothing -> traverseMaps n ms

rangeIndex :: Range -> Int -> Maybe Int
rangeIndex (Range start end) n
  | start <= n && n <= end = Just (n - start)
  | otherwise = Nothing

parseSeeds :: Text -> [Int]
parseSeeds txt = map num . tail . T.words $ txt

genRange :: [Int] -> [Int]
genRange xs = concat [ [a..a+b-1] | (a:b:cs) <- divvy 2 2 xs ]

parseMap :: Text -> [Map]
parseMap txt =
  let content = T.lines . T.strip $ txt
      key = head . T.words . head $ content
      values = map (map num . T.words) . tail $ content
  in map parseMapLine values

parseMapLine :: [Int] -> Map
parseMapLine [a,b,c] = Map (Range a (a+c-1)) (Range b (b+c-1))



-- Cast text to int
num :: Text -> Int
num = either (error . show) fst
    . signed decimal

