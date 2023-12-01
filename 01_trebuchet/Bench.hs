module Main (main) where

import AdventOfCode (aocBench)
import Trebuchet (parseInput, part1, part2)

main :: IO ()
main = aocBench "01_trebuchet" parseInput part1 part2
