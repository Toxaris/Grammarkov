{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
-- | The Grammarkov embedded domain-specific language for
-- nondeterministically building a sequence.
module Text.Grammarkov
  ( -- * Describing Sequences
    Grammarkov
  , say
  , (<|>)
  , choose
    -- * State Space
  , State (State)
  , explore
    -- * Executing Sequence Descriptions
  , generateAll
  , generateRandom
  ) where

import Control.Monad.Logic
import Control.Monad.Operational

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
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
generateAll (Grammarkov p) = observeAll (go p) where
  go p = case view p of
    Return _ -> return []
    Say x :>>= k -> fmap (x :) (go (k ()))
    Choose ps :>>= k -> foldr interleave mzero [go (p >>= k) | p <- ps]

-- | Generate one sequence described by a Grammarkov sequence
-- builder. Nondeterministic choices are resolved randomly.
generateRandom :: Grammarkov e a -> IO [e]
generateRandom (Grammarkov p) = go p where
  go p = case view p of
    Return _ -> return []
    Say x :>>= k -> fmap (x :) (go (k ()))
    Choose ps :>>= k -> randomRIO (0, length ps - 1) >>= \i -> go (ps !! i >>= k)

-- | The state space described by a Grammarkov sequence builder.
data State e
  = State Bool (Map e (State e))
  deriving Show

instance Ord e => Monoid (State e) where
  mempty = State False mempty
  mappend (State f1 m1) (State f2 m2) = State f' m' where
    f' = f1 || f2
    m' = Map.unionWith mappend m1 m2

-- | Follow a random path through a state space.
markov :: RandomGen g => (s -> e -> (Integer, s)) -> (s -> Double) -> State e -> s -> g -> [e]
markov step finish (State _ m) s g = go m s g where
  go m s g =
    let
      nexts = [(p, (e, s, state)) | (e, state) <- Map.toList m, let (p, s) = step s e]
      n = length nexts
      k = sum (map fst nexts)
      (x, g') = randomR (0, k - 1) g
      (e, s', State f' m') = select x nexts
    in e : if f'
      then
        let
          (x, g'') = randomR (0, 1) g
        in
          if x < finish s' then [] else go m' s' g''
      else go m' s' g'

select :: Integer -> [(Integer, a)] -> a
select _ [(_, x)] = x
select n ((p, x) : rest)
  | n < p = x
  | otherwise = select (n - p) rest

-- | Generate the state space described by a Grammarkov sequence
-- builder.
explore :: Ord e => Grammarkov e a -> State e
explore (Grammarkov p) = go p where
  convert = undefined
  go p = case view p of
    Return _ -> State True Map.empty
    Say x :>>= k -> State False (Map.singleton x (go (k ())))
    Choose ps :>>= k -> mconcat [go (p >>= k) | p <- ps]
