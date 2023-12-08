module Main (main) where

import AdventOfCode (aocBench)
import GearRatios (parseInput, part1, part2)

main :: IO ()
main = aocBench "03_gear_ratios" parseInput part1 part2
