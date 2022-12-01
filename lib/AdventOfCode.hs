module AdventOfCode (Parser, aocExec, aocTest, aocBench) where

import Control.DeepSeq (NFData)
import Criterion.Main (bench, bgroup, defaultConfig, defaultMainWith, env, nf)
import Criterion.Types (csvFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec (Spec, beforeAll, describe, hspec, it, shouldBe)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

type Parser = Parsec Void T.Text

inputFile :: FilePath
inputFile = "input.txt"

parseOrFail :: Parser a -> T.Text -> a
parseOrFail parseInput input = case parse parseInput "" input of
  Left err -> error $ errorBundlePretty err
  Right input -> input

aocExec :: Parser a -> (a -> String) -> (a -> String) -> IO ()
aocExec parseInput part1 part2 = do
  fileExists <- doesFileExist inputFile
  putStrLn $ if fileExists then "Reading input from " ++ inputFile ++ " ..." else inputFile ++ " not found, using stdin ..."
  source <- if fileExists then TIO.readFile inputFile else TIO.getContents
  let input = parseOrFail parseInput source
  putStrLn $ "Puzzle answer for part 1: " ++ part1 input
  putStrLn $ "Puzzle answer for part 2: " ++ part2 input

aocTest :: String -> Parser a -> (a -> String, String) -> (a -> String, String) -> IO ()
aocTest dir parseInput (part1, expected1) (part2, expected2) = hspec . beforeAll (readTestInput dir parseInput) . describe dir $ do
  it ("returns " ++ expected1 ++ " for part 1") $ (`shouldBe` expected1) . part1
  it ("returns " ++ expected2 ++ " for part 2") $ (`shouldBe` expected2) . part2
  where
    readTestInput dir parseInput = do
      cwd <- getCurrentDirectory
      fileContents <- TIO.readFile (cwd </> dir </> inputFile)
      return $ parseOrFail parseInput fileContents

aocBench :: (NFData a) => String -> Parser a -> (a -> String) -> (a -> String) -> IO ()
aocBench dir parseInput part1 part2 = do
  cwd <- getCurrentDirectory
  defaultMainWith
    defaultConfig
    [ env benchEnv $ \ ~(rawInput, input) ->
        bgroup
          dir
          [ bench "parse input" $ nf (parseOrFail parseInput) rawInput,
            bench "part 1" $ nf part1 input,
            bench "part 2" $ nf part2 input
          ]
    ]
  where
    benchEnv = do
      cwd <- getCurrentDirectory
      fileContents <- TIO.readFile (cwd </> dir </> inputFile)
      return (fileContents, parseOrFail parseInput fileContents)
