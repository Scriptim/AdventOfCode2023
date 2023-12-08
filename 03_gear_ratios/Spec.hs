module Main (main) where

import AdventOfCode (aocTest)
import GearRatios (parseInput, part1, part2)

main :: IO ()
main = aocTest "03_gear_ratios" parseInput (part1, "532331") (part2, "82301120")
