module Main where

import ThreepennyGUI
import Shapes

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }


data Tetris = Tetris (Vector, Shape) Shape [Shape]

type Vector = (Int, Int)

wellSize :: (Int, Int)
wellSize   = (wellWidth, wellHeight)
wellWidth  = 10
wellHeight = 20

startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

vAdd :: Vector -> Vector -> Vector
(x1, y1) `vAdd` (x2, y2) = (x1 + x2, y1 + y2)

place :: (Vector, Shape) -> Shape
place (v, s) = shiftShape v s

prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_,s) w _) = 
  prop_Shape s && (shapeSize w == wellSize)
 
addWalls :: Shape -> Shape
addWalls (S rs) = S $ bbb : bRb ++ [bbb]
  where
    bbb   = replicate (w + 2) $ Just Black
    bRb   = map pendB rs
    w     = fst $ shapeSize (S rs)
    pendB = (++ [Just Black]) . (Just Black :)

drawTetris :: Tetris -> Shape
drawTetris (Tetris (v, p) w _) = addWalls . combine w $ shiftShape v p

move :: Vector -> Tetris -> Tetris
move v (Tetris (v1, p) w l) = Tetris (v2, p) w l
  where v2 = v1 `vAdd` v

tick :: Tetris -> Maybe (Int, Tetris)
tick t  | collision t2 = dropNewPiece t
        | otherwise    = Just(0,t2)
    where t2 = move (0,1) t

collision :: Tetris -> Bool
collision (Tetris ((x,y), p) w _) = overlap || hitL || hitR || hitB
  where 
    overlap = place ((x,y),p) `overlaps` w
    hitL    = y < 0
    hitR    = pW + x > wellWidth
    hitB    = pH + y > wellHeight
    (pW,pH) = shapeSize p  

movePiece :: Int -> Tetris -> Tetris
movePiece n t = if collision t2 then t else t2
  where t2 = move (n,0) t

rotate :: Tetris -> Tetris
rotate (Tetris (v, p) w l) = Tetris (v, p2) w l
  where p2 = rotateShape p

rotatePiece :: Tetris -> Tetris
rotatePiece t = if collision t2 then t else t2
  where t2 = rotate t

dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris (v, p) w l) = 
  if collision t2 then Nothing else Just (n,t2)
  where
    t2     = Tetris (startPosition, p2) w2 (tail l)
    p2     = head l
    (n,w2) = clearLines $ combine w $ place (v,p)

clearLines :: Shape -> (Int, Shape)
clearLines (S xss) = (n, s2)
  where
    cRs   = filter (not . any null) xss
    n     = length cRs 
    uncRs = filter (any null) xss
    s2    = shiftShape (0,n) (S uncRs)

startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, shape1) (emptyShape wellSize) supply
  where
    shape1:supply = map ((allShapes !!) . floor . (6.999*)) rs

stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris a t = case a of
  Rotate    -> Just (0,rotatePiece t)
  MoveLeft  -> Just (0,movePiece (-1) t)
  MoveRight -> Just (0,movePiece 1 t)
  _         -> tick t
