#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Char

main = do  
  contents <- readFile "input"
  let numbers = map parseNumber $ lines $ textManip (replaceNums) contents 
  print numbers
  print $ sum numbers


-- part 1
parseNumber :: String -> Int
parseNumber = read . firstLast . filter (isDigit) 

firstLast xs = [head xs, last xs]


-- part 2
textManip f = T.unpack . f . T.pack
replaceNums txt =
  let mapping = [("oneight","18"),("threeight","38"),("fiveight","58"),("nineight","98"),
                 ("eightwo","82"),("eighthree","83"),("twone","21"),("sevenine","79"),
                 ("one","1"),("two","2"),("three","3"),
                 ("four","4"),("five","5"),("six","6"),
                 ("seven","7"),("eight","8"),("nine","9")]
  in foldl (replace') txt mapping 
  where replace' txt (a,b) = T.replace a b txt

