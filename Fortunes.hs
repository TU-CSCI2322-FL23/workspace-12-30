module Fortunes where

import Data.List
import Data.Char
import System.IO
import Debug.Trace
import System.Environment


main :: IO ()
main = 
  do args <- getArgs
     {-let fname = 
       case args of
         (x:xs) -> x
         [] -> "fortunes.txt"-}
     if "--help" `elem` args
     then putStrLn "Fortunes <filename>\nInteractive fortune telling app. Filename is optional."
     else
       do let fname = if null args then "fortunes.txt" else last args
          contents <- readFile (traceShowId fname)
          let fortunes = lines contents
          name <- prompt "What is your name: "
          let allFortunes = drop (indexOfName name) (cycle fortunes)
          tellFortunes name allFortunes

tellFortunes :: String -> [String] -> IO ()
tellFortunes name fortunes = 
              do putStr $ "Hello, " ++ name ++ ". Your fortune is: "
                 putStrLn $ head fortunes
                 resp <- prompt "Do you want another fortune? "
                 if map toLower resp `elem` ["y", "yes", "yes sir", "ye", "yup", "sure"]
                 then tellFortunes name (tail fortunes) 
                 else return ()

prompt :: String -> IO String
prompt question = 
  do putStr question
     hFlush stdout
     answer <- getLine
     return answer 
  
indexOfName :: String -> Int
indexOfName name = sum $ map ord name

getFortune :: [String] -> Int -> String
getFortune fortunes index = (cycle fortunes) !! index
  

