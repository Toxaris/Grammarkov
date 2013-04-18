{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
-- | The Grammarkov embedded domain-specific language for
-- nondeterministically building a sequence.
module Text.Grammarkov
  ( -- * Describing Sequences
    Grammarkov
  , say
  , (<|>)
  , choose
    -- * Executing Sequence Descriptions
  , generateAll
  , generateRandom
  ) where

import Control.Monad.Operational

import Data.String

import System.Random

-- | The Grammarkov monad for nondeterministically building
-- a sequence. The elements of sequences are of type 'e'.
newtype Grammarkov e a = Grammarkov (Program (Instruction e) a)
  deriving (Functor, Monad)

instance (a ~ (), IsString e) => IsString (Grammarkov e a) where
  fromString s = say (fromString s)

data Instruction e a where
  Say :: e -> Instruction e ()
  Choose :: [Program (Instruction e) a] -> Instruction e a

-- | Append an element to the sequence.
say :: e -> Grammarkov e ()
say x = Grammarkov (singleton (Say x))

-- | Choose nondeterministically from a list of Grammarkov
-- sequence builders.
choose :: [Grammarkov e a] -> Grammarkov e a
choose ps = Grammarkov (singleton (Choose [p | Grammarkov p <- ps]))

-- | Choose nondeterministically from two Grammarkov sequence
-- builders.
(<|>) :: Grammarkov e a -> Grammarkov e a -> Grammarkov e a
p <|> q = choose [p, q]

-- | Generate all sequences described by a Grammarkov sequence
-- builder.
generateAll :: Grammarkov e a -> [[e]]
generateAll (Grammarkov p) = go p where
  go p = case view p of
    Return _ -> return []
    Say x :>>= k -> fmap (x :) (go (k ()))
    Choose ps :>>= k -> ps >>= \p -> go (p >>= k)

-- | Generate one sequence described by a Grammarkov sequence
-- builder. Nondeterministic choices are resolved randomly.
generateRandom :: Grammarkov e a -> IO [e]
generateRandom (Grammarkov p) = go p where
  go p = case view p of
    Return _ -> return []
    Say x :>>= k -> fmap (x :) (go (k ()))
    Choose ps :>>= k -> randomRIO (0, length ps - 1) >>= \i -> go (ps !! i >>= k)

