module Main where

import System.IO
import Parser
import ParseProg

read_file :: String -> IO String
read_file file = do input <- openFile file ReadMode
                    program <- hGetContents input
                    return program

comparison :: [(Program Name, Name)] -> Program Name
comparison []              = error "No parse!"
comparison [(program, [])] = program
comparison [(_, input)]    = error ("Does not use all input: " ++ input)

main :: IO (Program Name)
main = do file <- getLine
          input <- read_file file
          return (comparison (parse parseProg input))
