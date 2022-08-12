module Base where

import qualified Data.List as L

-- Basic declarations
----------------------
type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

type Filled = [Value]

-- Basic definitions
---------------------

boxsize :: Int
boxsize =  3

values :: [Value]
values = ['1'..'9']

emptyValue :: Value
emptyValue = '.'

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _   = False

-- Example Grids
-----------------

easy :: Grid
easy = ["2....1.38",
        "........5",
        ".7...6...",
        ".......13",
        ".981..257",
        "31....8..",
        "9..81..2.",
        ".5..69784",
        "4..25...."]

gentle :: Grid
gentle = [".1.42...5",
          "..2.71.39",
          ".......4.",
          "2.71....6",
          "....4....",
          "6....74.3",
          ".7.......",
          "12.73.5..",
          "3...82.7."]

diabolical:: Grid
diabolical = [".9.7..86.",
              ".31..5.2.",
              "8.6......",
              "..7.5...6",
              "...3.7...",
              "5...1.7..",
              "......1.9",
              ".2.6..35.",
              ".54..8.7."]

unsolvable :: Grid
unsolvable =  ["1..9.7..3",
               ".8.....7.",
               "..9...6..",
               "..72.94..",
               "41.....95",
               "..85.43..",
               "..3...7..",
               ".5.....4.",
               "2..8.6..9"]

minimal :: Grid
minimal = [".98......",
           "....7....",
           "....15...",
           "1........",
           "...2....9",
           "...9.6.82",
           ".......3.",
           "5.1......",
           "...4...2."]

blank :: Grid
blank = replicate n $ replicate n emptyValue
    where n = boxsize ^ 2

-- Extracting rows, columns and boxes
---------------------------------------

-- Row Property: rows . rows = id
rows :: Matrix a -> [Row a]
rows = id

-- Col Property: cols . cols = id
cols :: Matrix a -> [Row a]
cols = L.transpose

-- Box Property: boxes . boxes = id
boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
    where
      pack   = split . map split
      split  = chop boxsize
      unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)


-- Properties of matrices
-------------------------

complete :: Matrix Filled -> Bool
complete = all $ all single

void :: Matrix Filled -> Bool
void = any $ any null

safe :: Matrix Filled -> Bool
safe m = all consistent (rows m) &&
         all consistent (cols m) &&
         all consistent (boxes m)

consistent :: Row Filled -> Bool
consistent = noDuplicates . concat . filter single

blocked :: Matrix Filled -> Bool
blocked m = void m || not (safe m)


-- Validity checking
---------------------

noDuplicates :: Eq a => [a] -> Bool
noDuplicates []     = True
noDuplicates (x:xs) = x `notElem` xs && noDuplicates xs

valid :: Grid -> Bool
valid g = all noDuplicates (rows g) &&
          all noDuplicates (cols g) &&
          all noDuplicates (boxes g)

-- Solver
-----------------


solve :: Grid -> Grid
solve = head . search . prune . fillup

collapse :: Matrix [a] -> [Matrix a]
collapse m = cartesianProduct $ map cartesianProduct m

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct []       = [[]]
cartesianProduct (xs:xss) = [x:ys | x <- xs, ys <- cartesianProduct xss]

search :: Matrix Filled -> [Grid]
search m 
    | blocked m = []
    | complete m = collapse m
    | otherwise = [g | m' <- expand m
                     , g  <- search (prune m')]
    
expand :: Matrix Filled -> [Matrix Filled]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
       (rows1,row:rows2) = span (all single) m
       (row1,cs:row2)    = span single row

fillup :: Grid -> Matrix Filled
fillup = map (map fill)
    where fill x = if x == emptyValue
                   then values
                   else [x]

-- Pruning
-----------

prune :: Matrix Filled -> Matrix Filled
prune = pruneBy boxes . pruneBy cols . pruneBy rows
    where
      pruneBy f = f . map reduce . f

reduce :: Row Filled -> Row Filled
reduce xss = [xs `minus` singles | xs <- xss]
    where singles = concat $ filter single xss

minus :: Filled -> Filled -> Filled
xs `minus` ys = if single xs then xs else xs L.\\ ys


-- Testing
------------

print' :: Grid -> IO ()
print' = putStrLn . unlines

main  :: IO ()
main = putStrLn . unlines $ solve minimal