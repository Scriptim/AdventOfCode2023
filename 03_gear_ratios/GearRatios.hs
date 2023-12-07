module GearRatios (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Data.Either (partitionEithers)
import Text.Megaparsec (SourcePos (sourceColumn, sourceLine), getSourcePos, sepEndBy, skipMany, unPos, (<|>))
import Text.Megaparsec.Char (char, newline, printChar)
import Text.Megaparsec.Char.Lexer (decimal)

type Position = (Int, Int)

type Number = (Int, Position)

type Symbol = (Char, Position)

parseInput :: Parser ([Number], [Symbol])
parseInput = partitionEithers <$> (blank *> (number <|> symbol) `sepEndBy` blank)
  where
    blank = skipMany (char '.' <|> newline) :: Parser ()
    number = Left <$> ((,) <$> decimal <*> position) :: Parser (Either Number Symbol)
    symbol = Right <$> ((,) <$> printChar <*> position) :: Parser (Either Number Symbol)
    position = (\pos -> (unPos $ sourceLine pos, unPos $ sourceColumn pos)) <$> getSourcePos :: Parser Position

adjacent :: Number -> Symbol -> Bool
adjacent (num_val, (num_row, num_col)) (_, (sym_row, sym_col)) = horizontal_dist <= 1 && vertical_dist <= 1
  where
    horizontal_dist = minimum $ [abs $ sym_col - col | col <- [num_col - num_width + 1 .. num_col]]
    vertical_dist = abs $ sym_row - num_row
    num_width = length $ show num_val

partNumbers :: ([Number], [Symbol]) -> [Number]
partNumbers (numbers, symbols) = filter (\num -> any (adjacent num) symbols) numbers

gears :: ([Number], [Symbol]) -> [[Number]]
gears (numbers, symbols) = filter ((== 2) . length) adjacent_nums
  where
    adjacent_nums = map (\asterisk -> filter (`adjacent` asterisk) numbers) asterisks
    asterisks = filter ((== '*') . fst) symbols

part1 :: ([Number], [Symbol]) -> String
part1 = show . sum . map fst . partNumbers

part2 :: ([Number], [Symbol]) -> String
part2 = show . sum . map (product . map fst) . gears
