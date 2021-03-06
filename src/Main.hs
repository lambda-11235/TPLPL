
module Main where

import Data
import Query as Q
import Unify
import Lexer
import Parser as P

import Control.Exception
import Data.Foldable (foldlM, foldrM)
import Data.List (findIndex, intersperse)
import qualified Data.Map as M
import Control.Monad (guard)
--import qualified System.Console.Readline as RL
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Parsec.Prim


main :: IO ()
main = do args <- getArgs
          let query = if elem "--depth-first" args then queryDF else queryBF
          let files = filter (\s -> null s || head s /= '-') args
          rules <- loadFiles files
          seq rules (repl query rules)

repl :: QueryControl -> Rules -> IO a
repl query rules =
  do putStr ":- "
     hFlush stdout
     str <- getLine
     toks <- catch (evaluate (scan str)) (\e -> print (e :: ErrorCall) >> repl query rules)
     if null toks then return () else
       case runParser P.query () "REPL" toks of
         Left err -> print err

         Right qs ->
           ppResults (query rules qs)
     repl query rules


loadFiles :: [String] -> IO Rules
loadFiles = foldlM loadFile M.empty

loadFile :: Rules -> String -> IO Rules
loadFile rules file = do contents <- readFile file
                         case runParser P.rules () file (scan contents) of
                           Left err -> error (show err)
                           Right rs -> return (foldl add' rules rs)
  where
    add' rules (name, params, subgoals) =
      let r = [Rule params subgoals] in
          case M.lookup name rules of
            Nothing -> M.insert name r rules
            Just rs -> M.insert name (rs ++ r) rules


ppResults :: [Unification] -> IO ()
ppResults us = ppResults' (map (M.filterWithKey goodId) us)
  where
    goodId (Id _ n) _ = n == 0

ppResults' [] = putStrLn "false"
ppResults' (u:us) = do ppRes u
                       s <- getLine
                       if s == ";" then
                         ppResults' (filter (/= u) us)
                       else
                         return ()
  where
    ppRes u =
      do let ss = intersperse ", " $ do (id, x) <- M.toList u
                                        return (prettyPrint (Var id) ++ " = " ++ prettyPrint x)
         if null ss then putStr "true" else mapM_ putStr ss
         putStrLn ""
