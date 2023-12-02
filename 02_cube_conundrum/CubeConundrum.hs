{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CubeConundrum (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.DeepSeq (NFData)
import Data.Text (pack)
import GHC.Generics (Generic)
import Text.Megaparsec (endBy1, sepBy1, (<|>))
import Text.Megaparsec.Char (char, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

data CubeColor = Red | Green | Blue deriving (Eq, Enum, Generic, NFData)

type Cubes = (CubeColor, Int)

type CubeSet = [Cubes]

type Game = (Int, [CubeSet])

parseInput :: Parser [Game]
parseInput = ((,) <$> game_id <* space <*> cube_sets) `endBy1` newline
  where
    game_id = string (pack "Game") *> space *> decimal <* char ':' :: Parser Int
    cube_sets = cube_set `sepBy1` (char ';' *> space) :: Parser [CubeSet]
    cube_set = cubes `sepBy1` (char ',' *> space) :: Parser CubeSet
    cubes = flip (,) <$> decimal <* space <*> cube_color :: Parser Cubes
    cube_color = Red <$ string (pack "red") <|> Green <$ string (pack "green") <|> Blue <$ string (pack "blue") :: Parser CubeColor

maxCubes :: Cubes -> Bool
maxCubes (Red, n) = n <= 12
maxCubes (Green, n) = n <= 13
maxCubes (Blue, n) = n <= 14

requiredCubes :: [Cubes] -> [Int]
requiredCubes cubes = map maxByColor [Red ..]
  where
    maxByColor color = maximum . map snd . filter ((== color) . fst) $ cubes

part1 :: [Game] -> String
part1 = show . sum . map fst . filter (all (all maxCubes) . snd)

part2 :: [Game] -> String
part2 = show . sum . map (product . requiredCubes . concat . snd)
