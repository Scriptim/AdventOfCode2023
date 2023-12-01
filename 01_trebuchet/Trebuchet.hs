module Trebuchet (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Data.List (findIndex, isPrefixOf)
import Data.Char (digitToInt, isDigit)
import Text.Megaparsec (endBy1, some)
import Text.Megaparsec.Char (alphaNumChar, newline)

digitNames :: [String]
digitNames = ["_", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

parseInput :: Parser [String]
parseInput = some alphaNumChar `endBy1` newline

digits :: String -> [Int]
digits = map digitToInt . filter isDigit

digitsWithNames :: String -> [Int]
digitsWithNames [] = []
digitsWithNames str@(x : xs)
  | isDigit x = digitToInt x : digitsWithNames xs
  | otherwise = case findIndex (`isPrefixOf` str) digitNames of
      Just n -> n : digitsWithNames xs
      Nothing -> digitsWithNames xs

lineValue :: [Int] -> Int
lineValue xs = 10 * head xs + last xs

part1 :: [String] -> String
part1 = show . sum . map (lineValue . digits)

part2 :: [String] -> String
part2 = show . sum . map (lineValue . digitsWithNames)
