{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment

import Text.Grammarkov

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--all"] -> mapM_ printText (generateAll start)
    ["--random"] -> generateRandom start >>= printText
    _ -> printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "USAGE:"
  putStrLn "  grammarkov --all       prints all variants."
  putStrLn "  grammarkov --random    prints a randomly chosen variant."

printText :: [String] -> IO ()
printText = go 1 where
  go _ [] = putStrLn ""
  go _ [w] = putStr w >> putStrLn "."
  go n (w : ws) =
      if n' < 65
        then putStr w >> putStr " " >> go n' ws
        else putStrLn "" >> go 1 (w : ws)
    where n' = n + length w

start = sentence >> "and" >> sentence

sentence
   =   indirect person (action person (adjective >> object))

adjective
   =   "stupid"
  <|>  "clever"
  <|>  "annoying"
  <|>  "interesting"
  <|>  "additional"
  <|>  "exiting"
  <|>  "impossible"
  <|>  "easy"
  <|>  "hard"

object
   =   ("markov" >> "models")
  <|>  ("text" >> "generation")
  <|>  ("side" >> "projects")

action x y
   =   (x >> ("hates" <|> "loves" <|> "considers" <|> "does" <|> "likes") >> y)
  <|>  (x >>
        ("doesn't" <|> "cannot" <|> ("would" >> "not")) >>
        ("hate" <|> "love" <|> "consider" <|> "do" <|> "like") >>
        y )

person
   =   "Paolo"
  <|>  ("Jona" <|> "Jonathan")
  <|>  "Tillmann"

indirect x y
   =   (x >> expresses >> "that" >> y)
  <|>  (y >> "or" >> "so" >> x >> expresses)

expresses
   =   "says"
  <|>  "believes"
  <|>  "figures"
  <|>  "thinks"
  <|>  "assumes"
