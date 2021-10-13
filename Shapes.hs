module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck


type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

data Shape = S [Row] deriving (Eq)
type Row   = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs


showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]

    showSquare Nothing      = '.'
    showSquare (Just Black) = '#'
    showSquare (Just Grey)  = 'g'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes]
   where
      makeSquares = map (map colour)
      colour c    = lookup c [ ('I', Red), ('J', Grey), ('T', Blue)
                             , ('O', Yellow), ('Z',Cyan), ('L', Green)
                             , ('S', Purple) ]
      shapes =
              [["I",
                "I",
                "I",
                "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

emptyShape :: (Int, Int) -> Shape
emptyShape (w, h) = S . replicate h $ replicate w Nothing

shapeSize :: Shape -> (Int,Int)
shapeSize (S []) = (0,0)
shapeSize (S rs) = (length $ head rs, length rs)

blockCount :: Shape -> Int
blockCount = length . filter (not . isNothing) . concat . rows

prop_Shape :: Shape -> Bool
prop_Shape (S rs) = isRect && noNulls
  where
    noNulls = not (null rs) && not (any null rs)
    isRect  = all ((length (head rs) ==) . length) rs

genColour :: Gen Colour
genColour =
  elements [ Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey ]

instance Arbitrary Colour where
  arbitrary = genColour

genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape

rotateShape :: Shape -> Shape
rotateShape = S . map reverse . transpose . rows

shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (x,y) (S rs) = S $ uShift ++ rShift
  where
    rShift = map (replicate x Nothing ++) rs
    uShift = rows $ emptyShape (x + initW, y)
    initW  = fst $ shapeSize (S rs)

padShape :: (Int, Int) -> Shape -> Shape
padShape (x,y) (S rs) = S $ lShift ++ dShift
  where
    lShift = map (++ replicate x Nothing) rs
    dShift = rows $ emptyShape (x + initW, y)
    initW  = fst $ shapeSize (S rs)

padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (w,h) s = padShape (w - w', h - h') s
  where (w',h') = shapeSize s

overlaps :: Shape -> Shape -> Bool
(S xss) `overlaps` (S yss) = any colliding squares
  where 
    colliding = all (not . isNothing)
    squares   = concat $ zipWith (zipWith toLst) xss yss
    toLst a b = [a,b]

zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (S xss) (S yss) = S $ zipWith (zipWith f) xss yss

combine :: Shape -> Shape -> Shape
s1 `combine` s2 = zipShapeWith fltSqs s1' s2'
  where
    [s1',s2']  = map (padShapeTo (maxW,maxH)) [s1,s2]
    fltSqs a b = if isNothing a then b else a
    maxW       = if w1 > w2 then w1 else w2
    maxH       = if h1 > h2 then h1 else h2
    (w1,h1)    = shapeSize s1
    (w2,h2)    = shapeSize s2