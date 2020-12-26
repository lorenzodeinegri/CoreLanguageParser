module Main where

import System.IO
import Parser
import ParseProg

read_file :: IO String
read_file = do input <- openFile "input.txt" ReadMode
               program <- hGetContents input
               return program

comparison :: [(Program Name, Name)] -> Program Name
comparison []              = error "No parse!"
comparison [(program, [])] = program
comparison [(_, input)]    = error ("Does not use all input: " ++ input)

main :: IO (Program Name)
main = do input <- read_file
          return (comparison (parse parseProg input))
