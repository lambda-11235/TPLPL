
module Unify where

import Control.Monad (guard)
import Data.Map as M
import qualified Data.Set as S

import Data


type Unification = Map Id Data


unifyMany :: Unification -> [(Data, Data)] -> Maybe Unification
unifyMany m [] = Just m
unifyMany m ((x, y):ps) = do m' <- unify m x y
                             unifyMany m' ps


unify :: Unification -> Data -> Data -> Maybe Unification
unify m (Var s) y =
  if occurs s y then Nothing else
    case M.lookup s m of
      Just x -> unify m x y
      Nothing -> return $ insert s y m
unify m x (Var s) =
  if occurs s x then Nothing else
    case M.lookup s m of
      Just y -> unify m x y
      Nothing -> return $ insert s x m
unify m (Struct s1 []) (Struct s2 []) =
  do guard (s1 == s2)
     return m
unify m (Struct s1 (x:xs)) (Struct s2 (y:ys)) =
  do guard (s1 == s2)
     m' <- unify m x y
     unify m' (Struct s1 xs) (Struct s2 ys)
unify _ _ _ = Nothing


occurs :: Id -> Data -> Bool
occurs s (Var t) = s == t
occurs s (Struct _ xs) = any (occurs s) xs


simplify :: Unification -> Maybe Unification
simplify m = let xs = M.toList (M.map (simp m S.empty) m) in
                 M.fromList <$> (mapM (mapM id) xs)
  where
    simp :: Unification -> S.Set Id -> Data -> Maybe Data
    simp m occ (Var s) =
      if S.member s occ then Nothing else
        case M.lookup s m of
          Just x -> simp m (S.insert s occ) x
          Nothing -> return (Var s)
    simp m occ (Struct t xs) = Struct t <$> (mapM (simp m occ) xs)
