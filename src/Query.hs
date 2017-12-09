
module Query ( Rule (..)
             , Query
             , Rules
             , query
             , queryBF)
               where

import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Data
import Unify


-- | A rule is a list of parameters and a list of subgoals.
data Rule = Rule [Data] [Data]
  deriving (Eq, Show)

type Query = [Data]
type Rules = M.Map String [Rule]


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


-- | Like query, but tries breadth first.
queryBF :: Rules -> Query -> [Unification]
queryBF rules qs = queryBF' 1 rules [(qs, M.empty)]

-- The third argument is a list of goals with their unifications. The general
-- technique here is to find all possible rule matches for all goals, check if
-- any result in success for the goal, and then repeat.
queryBF' :: Int -> Rules -> [(Query, Unification)] -> [Unification]
queryBF' d rules qss =
  let qss' = qss >>= (\(qs, u) -> flat u [] qs)
      succ = filter ((== 0) . length . fst) qss'
      cont = filter ((/= 0) . length . fst) qss'
  in
    fmap snd succ ++ queryBF' (d + 1) rules cont
  where
    flat :: Unification -> Query -> Query -> [(Query, Unification)]
    flat u qs [] = []
    flat u qs1 (q:qs2) =
      let t = fmap (\(sgs, u') -> (qs1 ++ sgs ++ qs2, u')) (matches d u rules q) in
          t ++ (flat u (qs1 ++ [q]) qs2)


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
