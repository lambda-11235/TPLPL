
module Query ( Rule (..)
             , Query
             , Rules
             , query)
               where

import Data.Map as M
import Data.Maybe (catMaybes)

import Data
import Unify


-- | A rule is a list of parameters and a list of subgoals.
data Rule = Rule [Data] [Data]
  deriving (Eq, Show)

type Query = [Data]
type Rules = Map String [Rule]


-- | Performs a query with the given rules. Returns a list of possible
-- unifications. An empty list denotes failure.
query :: Rules -> Query -> [Unification]
query = query' 1 M.empty

-- The first argument is used to generate unique variable names for matched
-- rules. The basic operation of this algorithm is to match the first subgoal to
-- a rule, expand it, and repeat until we have no subgoals.
query' :: Int -> Unification -> Rules -> Query -> [Unification]
query' d u rules [] = [u]
query' d u rules (q:qs) =
  do (subgoals, u') <- matches d u rules q
     u'' <- query' (d + 1) u' rules (subgoals ++ qs)
     return (simplify u'')


-- Finds all matches to a subgoal, and return them as a list of subgoals and
-- unification results.
matches :: Int -> Unification -> Rules -> Data -> [([Data], Unification)]
matches d u rules (Var s) =
  case M.lookup s u of
    Nothing -> []
    Just y -> matches d u rules y
matches d u rules (Struct s xs) =
  case M.lookup s rules of
    Nothing -> []
    Just ys -> catMaybes $ fmap (flip tryMatch xs) ys
  where
    -- | Convert all variables in rule to unambiguous forms.
    convert (Var (Id s _)) = Var (Id s d)
    convert (Struct s xs) = Struct s (fmap convert xs)

    tryMatch (Rule ms sgoals) xs =
      let ms' = fmap convert ms
          sgoals' = fmap convert sgoals
      in
        if length ms' /= length xs then Nothing else
          case unifyMany u (zip ms' xs) of
            Nothing -> Nothing
            Just u -> Just (sgoals', u)
