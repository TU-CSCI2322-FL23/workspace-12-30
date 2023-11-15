module Fortunes where

import Data.List
import Data.Char
import System.IO
import Debug.Trace
import System.Environment
import System.Console.GetOpt

data Flag = Help | Quick | Number String | Start String deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['q'] ["quick", "quiet"] (NoArg Quick) "Print the fortune(s) and exit."
          , Option ['n'] ["num"] (ReqArg Number "<num>") "Print <num> fortunes at a time."
          , Option ['s'] ["start"] (ReqArg Start "<num>") 
                  "Start at fortune <num>, defaults to a value based on the name, or 1 for quick."
          ]


main :: IO ()
main = 
  do args <- getArgs
     let (flags, inputs, errors) = getOpt Permute options args
     {-let fname = 
       case args of
         (x:xs) -> x
         [] -> "fortunes.txt"-}
     if Help `elem` flags || not (null errors)
     then putStrLn $ usageInfo "Fortunes [options] [filename]\nInteractive fortune telling app." options
     else
       do let fname = if null inputs then "fortunes.txt" else head inputs
          contents <- readFile (fname)
          let fortunes = cycle $ lines contents
          if Quick `elem` flags
          then tellFortunesQuick flags fortunes
          else
            do name <- prompt "What is your name: "
               let allFortunes = drop (indexOfName name) fortunes
               tellFortunes name allFortunes

tellFortunesQuick :: [Flag] -> [String] -> IO ()
tellFortunesQuick flags fortunes = 
  let index = startOfFlags flags
  in putStrLn $ head $ drop index fortunes

startOfFlags :: [Flag] -> Int
startOfFlags [] = 1
startOfFlags (Start x:flags) = read x
startOfFlags (f:flags) = startOfFlags flags

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
  

