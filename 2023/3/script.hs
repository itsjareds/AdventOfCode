#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle pkgs.matrix ])"

{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Read
import Data.Char
import qualified Data.Text as T

data Point = Point Int Int deriving (Show)

main = do
  content <- readFile "input"

  let schematic = T.lines . T.pack $ content
  --print schematic
  
  let nums = concat
           . map parseNums
           . zip [1..]
           $ schematic
  --print nums

  let symbols = concat
              . map parseSymbols 
              . zip [1..]
              $ schematic
  --print symbols

  let hits = map (collisions nums) symbols
  --print hits

  let gears = filter getGears
            $ hits
  --print gears

  let ratios = map getRatios
             $ gears
  --print ratios
  print . sum $ ratios



getRatios (_, ns) = product . map (\(_, _, n) -> num n) $ ns

getGears ((_, symbol), ns) = symbol=="*" && length ns == 2

collisions nums (p, symbol) = ((p,symbol), [ n | n <- nums, collision p n ])

collision :: Point -> (Point, Point, T.Text) -> Bool
collision (Point x y) (Point x1 y1, Point x2 y2, _) = and [x1<=x, x<=x2, y1<=y, y<=y2]

parseNums nums =
  let f = isDigit
      g = tag y
      y = fst nums
      txt = snd nums
  in [ (x,y,n) | (x,y,n) <- breakValues' f g txt, f . T.head $ n ]

tag x y txt = (Point (x-1) (y-1), Point (x+1) (y + (T.length txt)), txt)
tag' x y txt = (Point x y, txt)

parseSymbols v =
  let f = isSymb
      g = tag' x
      x = fst v
      txt = snd v
  in [ (p,n) | (p,n) <- breakValues' f g txt, f . T.head $ n ]

isSymb c = (not . isDigit) c && c /= '.'

breakValues' f g txt =
  let f' = if f (T.head txt) then not . f else f
  in breakValues f' g 1 txt

breakValues :: (Char -> Bool) -> (Int -> T.Text -> a) -> Int -> T.Text -> [a]
breakValues f g n txt
  | T.null txt = []
  | otherwise =
    let v = T.break f txt
        match = fst v
        remainder = snd v
    in [g n $ match] ++ breakValues (not . f) g ((+n) $ T.length match) remainder

-- Cast text to int
num :: T.Text -> Int
num = either (error . show) fst
    . signed decimal

