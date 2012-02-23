module QRDraw
( Bitmap2D
, blankImage
, addAlignmentPattern
, addTypeInformation
, addTimingPattern
, addBlackPixel
, addData
) where

import Data.Array
import QREncode

type Bitmap2D = Array (Int,Int) Bool
type Pixel = ((Int,Int),Bool)
data Mask = MaskNone | Mask0 | Mask1 | Mask2 | Mask3 | Mask4 | Mask5 | Mask6 | Mask7

blankImage :: Int -> Int -> Bitmap2D
blankImage width height = array ((0,0),(width-1,height-1)) [((x,y),False) | x <- [0..width-1], y <- [0..height-1]]

addAlignmentPattern :: Int -> Int -> Bitmap2D -> Bitmap2D
addAlignmentPattern x y bitmap = 
    bitmap // (zip alignmentPattern (repeat True))
    where alignmentPattern = range ((x,y),(x+6,y)) ++ range ((x,y+1),(x,y+5)) ++ range ((x,y+6),(x+6,y+6)) ++ range ((x+6,y+1),(x+6,y+5)) ++ range ((x+2,y+2),(x+4,y+4))

addTimingPattern :: (Int, Int) -> (Int, Int) -> Bitmap2D -> Bitmap2D
addTimingPattern start end bitmap =
    bitmap // (zip timingPattern (cycle [True, False]))
    where timingPattern = range (start,end)

addTypeInformation :: Bitmap2D -> Bitmap2D
addTypeInformation bitmap = (draw formatPath1 (draw formatPath2 bitmap))
    where draw = drawPixelsOnPath MaskNone typeInformation

addBlackPixel :: Bitmap2D -> Bitmap2D
addBlackPixel bitmap = bitmap // [((8,13),True)]

addData :: Bitmap2D -> Bitmap2D
addData = drawPixelsOnPath Mask0 dataBits dataPattern

drawPixelsOnPath :: Mask -> [Bool] -> [(Int, Int)] -> Bitmap2D -> Bitmap2D

drawPixelsOnPath mask pixels path bitmap = bitmap // (map (applyMask mask) (zip path pixels))

applyMask :: Mask -> Pixel -> Pixel

applyMask mask (position,bit)
    | maskCondition mask position == 0 = (position,not bit)
    | otherwise = (position,bit)

maskCondition :: Mask -> (Int,Int) -> Int
maskCondition MaskNone _ = 1
maskCondition Mask0 (x,y) = (y + x) `mod` 2
maskCondition Mask1 (x,y) = y `mod` 2
maskCondition Mask2 (x,y) = x `mod` 3
maskCondition Mask3 (x,y) = (y + x) `mod` 3
maskCondition Mask4 (x,y) = ((y `div` 2) + (x `div` 3)) `mod` 2
maskCondition Mask5 (x,y) = ((y * x) `mod` 2) + ((y * x) `mod` 3)
maskCondition Mask6 (x,y) = (((y * x) `mod` 2) + ((y * x) `mod` 3)) `mod` 2
maskCondition Mask7 (x,y) = (((y + x) `mod` 2) + ((y * x) `mod` 3)) `mod` 2

------------------------------------------------------------
typeInformation = [False, True, True, False, True, False, True, False, True, False, True, True, True, True, True]

formatPath1 = [(0, 8), (1, 8), (2, 8), (3, 8), (4, 8), (5, 8), (7, 8), (8, 8), (8, 7), (8,5), (8,4), (8,3),(8,2),(8,1),(8,0)]

formatPath2 = [(8,20),(8,19),(8,18),(8,17),(8,16),(8,15),(8,14),(13,8),(14,8),(15,8),(16,8),(17,8),(18,8),(19,8),(20,8)]

dataBits = stringWithErrorCorrection 1 Q "HELLO WORLD"

upwardColumn = cycle [(-1,0),(1,-1)]
downwardColumn = cycle [(-1,0),(1,1)]

pathFrom end [] = [end]
pathFrom (x,y) ((dx,dy):deltas) = (x,y) : pathFrom (x+dx,y+dy) deltas

dataPattern = pathFrom (20, 20) $ (take 23 upwardColumn) ++ [(-1, 0)] ++ (take 23 downwardColumn) ++ [(-1,0)]  ++ (take 23 upwardColumn) ++ [(-1,0)] ++ (take 23 downwardColumn) ++ [(-1,0)] ++ (take 27 upwardColumn) ++ [(1,-2)] ++ (take 11 upwardColumn) ++ [(-1,0)] ++ (take 11 downwardColumn) ++ [(1,2)] ++ (take 27 downwardColumn) ++ [(-1,-8)] ++ (take 7 upwardColumn) ++ [(-2,0)] ++ (take 7 downwardColumn) ++ [(-1,0)] ++ (take 7 upwardColumn) ++ [(-1,0)] ++ (take 7 downwardColumn)

