module Base where

import qualified Data.List as L


type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

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
        
filled :: Grid
filled = ["123456789", 
          "234567891", 
          "345678912", 
          "456789123",
          "567891234",
          "678912345",
          "789123456",
          "891234567",
          "912345678"]

blank :: Grid
blank = replicate 9 $ replicate 9 '.'

rows :: Matrix a -> [Row a]
rows = id

-- Row Property: rows . rows = id

cols :: Matrix a -> [Row a]
cols = L.transpose

-- Col Property: cols . cols = id

boxes :: Matrix a -> [Row a]
boxes = map concat . merge3Elements . concat . L.transpose . map merge3Elements

-- Box Property: boxes . boxes = id


noDuplicates :: Eq a => [a] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = x `notElem` xs && noDuplicates xs

valid :: Grid -> Bool
valid g = all noDuplicates (rows g) && 
          all noDuplicates (cols g) && 
          all noDuplicates (boxes g)
          
          
merge3Elements :: [a] -> [[a]]
merge3Elements [] = []
merge3Elements (a:b:c:ds) = [a,b,c] : merge3Elements ds
merge3Elements as = [as]


solve :: Grid -> [Grid]
solve = filter valid . collapse . fillup

collapse :: Matrix [a] -> [Matrix a]
collapse m = cartesianProduct $ map cartesianProduct m

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs:xss) = [x:ys | x <- xs, ys <- cartesianProduct xss]

type Filled = [Value]

fillup :: Grid -> Matrix Filled
fillup = map (map fill)
    where fill x = if x == '.' 
                   then ['1'..'9'] 
                   else [x]