#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle pkgs.split ])"

{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split
import Data.Text.Read
import qualified Data.Map as M
import qualified Data.Text as T

main = do
  content <- readFile "input"

  -- Parse out each line into useful data types
  let games = map (parse) $ T.lines $ T.pack content

  print "Part 1"

  -- Check each round in each game and calculate if it's valid
  let gameChecks = map (map checkRound) games

  -- Filter games to only include games where all rounds were valid
  -- Use zip to assign the game id
  let validGames = [ x | (x,y) <- zip [1..] gameChecks, and y ]
   
  print validGames
  print $ sum validGames
  
  print "Part 2"

  -- Return a list of the calculated power for each game
  let gamePowers = map (power) games

  print gamePowers
  print $ sum gamePowers



parse = parseGame . parseLine 

-- Remove game prefix, e.g. "Game 1:"
parseLine :: T.Text -> T.Text
parseLine line = T.drop 1 $ snd $ T.breakOn ":" line

-- Parse a line into list of rounds, with each round being a list of cube drawings
parseGame :: T.Text -> [[(T.Text, Int)]]
parseGame str = map (parseRound . T.strip) $ T.splitOn ";" str

-- Parse a round into a list of (Color, Number) pairs, with each pair being how many cubes were drawn of a color
parseRound :: T.Text -> [(T.Text, Int)]
parseRound str = map (parseColor . T.strip) $ T.splitOn "," str

-- Parse a color string into its component parts, e.g. "3 green" -> ("green", 3)
parseColor :: T.Text -> (T.Text, Int)
parseColor xs =
  let w = T.words xs
  in (last w, num $ head w)

-- Cast text to int
num :: T.Text -> Int
num = either (error . show) fst
    . signed decimal

-- Return true if the round is valid according to the scenario rules
checkRound :: [(T.Text, Int)] -> Bool
checkRound cs = and $ map (checkPick) cs

checkPick :: (T.Text, Int) -> Bool
checkPick (color, amount)
  | color == "red"   = (amount <= 12)
  | color == "green" = (amount <= 13)
  | color == "blue"  = (amount <= 14)
  | otherwise = (error . show) (color, amount) 

-- Part 2

-- Flatten each game into a list of picks, then group the picks by (Color, max amount), then take the product of the amounts
power games = product . map (snd) $ group (max) [ (k,v) | (k,v) <- concat games ]
group f = M.toList . M.fromListWith f

