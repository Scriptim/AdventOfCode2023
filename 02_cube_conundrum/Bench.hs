module Main (main) where

import AdventOfCode (aocBench)
import CubeConundrum (parseInput, part1, part2)

main :: IO ()
main = aocBench "02_cube_conundrum" parseInput part1 part2
