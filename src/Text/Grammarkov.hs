{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
module Text.Grammarkov
  ( Grammarkov
  , say
  , (<|>)
  , choose
  , generateAll
  , generateRandom
  ) where

import Control.Monad.Operational

import Data.String

import System.Random

newtype Grammarkov e a = Grammarkov (Program (Instruction e) a)
  deriving (Functor, Monad)

instance (a ~ (), IsString e) => IsString (Grammarkov e a) where
  fromString s = say (fromString s)

data Instruction e a where
  Say :: e -> Instruction e ()
  Choose :: [Program (Instruction e) a] -> Instruction e a

say :: e -> Grammarkov e ()
say x = Grammarkov (singleton (Say x))

choose :: [Grammarkov e a] -> Grammarkov e a
choose ps = Grammarkov (singleton (Choose [p | Grammarkov p <- ps]))

(<|>) :: Grammarkov e a -> Grammarkov e a -> Grammarkov e a
p <|> q = choose [p, q]

generateAll :: Grammarkov e a -> [[e]]
generateAll (Grammarkov p) = go p where
  go p = case view p of
    Return _ -> return []
    Say x :>>= k -> fmap (x :) (go (k ()))
    Choose ps :>>= k -> ps >>= \p -> go (p >>= k)

generateRandom :: Grammarkov e a -> IO [e]
generateRandom (Grammarkov p) = go p where
  go p = case view p of
    Return _ -> return []
    Say x :>>= k -> fmap (x :) (go (k ()))
    Choose ps :>>= k -> randomRIO (0, length ps - 1) >>= \i -> go (ps !! i >>= k)

