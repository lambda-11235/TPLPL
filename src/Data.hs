
module Data where


-- | The integer is used to disinguish variables used in various rules.
data Id = Id String Int
  deriving (Eq, Ord, Show)

-- | A piece of data may be a variable or a struct. Atoms are considered to be 0
-- arity structs.
data Data = Var Id
          | Struct String [Data]
          deriving (Eq, Show)


var :: String -> Data
var s = Var (Id s 0)

atom :: String -> Data
atom s = Struct s []


prettyPrint :: Data -> String
prettyPrint (Var (Id s 0)) = "?" ++ s
prettyPrint (Var (Id s n)) = "?" ++ s ++ "_" ++ show n
prettyPrint (Struct s []) = s
prettyPrint (Struct s xs) = s ++ "(" ++ pp xs ++ ")"
  where
    pp [] = ""
    pp [x] = prettyPrint x
    pp (x:xs) = prettyPrint x ++ ", " ++ pp xs
