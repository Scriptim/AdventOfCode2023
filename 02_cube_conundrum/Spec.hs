module Main (main) where

import AdventOfCode (aocTest)
import CubeConundrum (parseInput, part1, part2)

main :: IO ()
main = aocTest "02_cube_conundrum" parseInput (part1, "3059") (part2, "65371")
