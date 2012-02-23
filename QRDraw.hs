module QRDraw
( Bitmap2D
, blankImage
, addAlignmentPattern
, addTypeInformation
, addData
) where

import Data.Array
import QREncode

type Bitmap2D = Array (Int,Int) Bool
type Pixel = ((Int,Int),Bool)
data Mask = MaskNone | Mask0

blankImage :: Int -> Int -> Bitmap2D
blankImage width height = array ((0,0),(width-1,height-1)) [((x,y),False) | x <- [0..width-1], y <- [0..height-1]]

addAlignmentPattern :: Int -> Int -> Bitmap2D -> Bitmap2D
addAlignmentPattern x y bitmap = 
    bitmap // (zip alignmentPattern (repeat True))
    where alignmentPattern = range ((x,y),(x+6,y)) ++ range ((x,y+1),(x,y+5)) ++ range ((x,y+6),(x+6,y+6)) ++ range ((x+6,y+1),(x+6,y+5)) ++ range ((x+2,y+2),(x+4,y+4))

addTypeInformation :: Bitmap2D -> Bitmap2D
addTypeInformation bitmap = (draw formatPath1 (draw formatPath2 bitmap))
    where draw = drawPixelsOnPath MaskNone typeInformation

addData :: Bitmap2D -> Bitmap2D
addData = drawPixelsOnPath Mask0 dataBits dataPattern

drawPixelsOnPath :: Mask -> [Bool] -> [(Int, Int)] -> Bitmap2D -> Bitmap2D

drawPixelsOnPath mask pixels path bitmap = bitmap // (map (applyMask mask) (zip path pixels))

applyMask :: Mask -> Pixel -> Pixel

applyMask MaskNone p = p
applyMask Mask0 ((x,y),bit)
    | (y + x) `mod` 2 == 0 = ((x,y),not bit)
    | otherwise = ((x,y),bit)


------------------------------------------------------------
typeInformation = [False, True, True, False, True, False, True, False, True, False, True, True, True, True, True]

formatPath1 = [(0, 8), (1, 8), (2, 8), (3, 8), (4, 8), (5, 8), (7, 8), (8, 8), (8, 7), (8,5), (8,4), (8,3),(8,2),(8,1),(8,0)]

formatPath2 = [(8,20),(8,19),(8,18),(8,17),(8,16),(8,15),(8,14),(13,8),(14,8),(15,8),(16,8),(17,8),(18,8),(19,8),(20,8)]

dataBits = stringWithErrorCorrection 1 Q "HELLO WORLD"

upwardColumn = cycle [(-1,0),(1,-1)]
downwardColumn = cycle [(-1,0),(1,1)]

pathFrom end [] = [end]
pathFrom (x,y) ((dx,dy):deltas) = (x,y) : pathFrom (x+dx,y+dy) deltas

dataPattern = pathFrom (20, 20) $ (take 23 upwardColumn) ++ [(-1, 0)] ++ (take 23 downwardColumn) ++ [(-1,0)]  ++ (take 23 upwardColumn) ++ [(-1,0)] ++ (take 23 downwardColumn) ++ [(-1,0)] ++ (take 27 upwardColumn) ++ [(1,-2)] ++ (take 11 upwardColumn) ++ [(-1,0)] ++ (take 11 downwardColumn) ++ [(1,2)] ++ (take 27 downwardColumn)

