module QRDraw
( Bitmap2D
, Mask (MaskNone , Mask0 , Mask1 , Mask2 , Mask3 , Mask4 , Mask5 , Mask6 , Mask7)
, base
, blankImage
, addAlignmentPattern
, addTypeInformation
, addTimingPattern
, addBlackPixel
, addData
, dataToBitmap
, dataToBitmapWithBestMask
, penaltyRule1
, scoreSequence
, penaltyRule2
, penaltyRule3
, penaltyRule4
, penalty
) where

import Data.Array
import Data.List
import Debug.Trace

import QRShared

type Bitmap2D = Array (Int,Int) Bool
type Pixel = ((Int,Int),Bool)
data Mask = MaskNone | Mask0 | Mask1 | Mask2 | Mask3 | Mask4 | Mask5 | Mask6 | Mask7 deriving (Eq, Show, Enum, Ord, Bounded)

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

addBlackPixel :: Bitmap2D -> Bitmap2D
addBlackPixel bitmap = bitmap // [((8,13),True)]

addTypeInformation :: ECCLevel -> Mask -> Bitmap2D -> Bitmap2D
addTypeInformation ecclevel mask bitmap = (draw formatPath1 (draw formatPath2 bitmap))
    where draw = drawPixelsOnPath MaskNone (typeInformation ecclevel mask)

base :: Bitmap2D
base = addTimingPattern (8,6) (12,6) .
       addTimingPattern (6,8) (6,12) .
       addBlackPixel .
       addAlignmentPattern 0 0 .
       addAlignmentPattern 0 14 .
       addAlignmentPattern 14 0 $
       blankImage 21 21

addData :: [Bool] -> Mask -> Bitmap2D -> Bitmap2D
addData dataBits mask = drawPixelsOnPath mask dataBits dataPattern

dataToBitmap dataBits ecclevel mask = addData dataBits mask (addTypeInformation ecclevel mask base)

dataToBitmapWithBestMask :: [Bool] -> ECCLevel -> Bitmap2D
dataToBitmapWithBestMask dataBits ecclevel = snd (head alternatives)
    where images       = map (dataToBitmap dataBits ecclevel) [Mask0 .. Mask7]
          scores       = map penalty images
          alternatives = sort (zip scores images)
--

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
{-typeInformation = [False, True, True, False, True, False, True, False, True, False, True, True, True, True, True]-}

typeInformation :: ECCLevel -> Mask -> [Bool]
typeInformation Q Mask0 = binaryFromHumanReadable "011010101011111"
typeInformation Q Mask1 = binaryFromHumanReadable "011000001101000"
typeInformation Q Mask2 = binaryFromHumanReadable "011111100110001"
typeInformation Q Mask3 = binaryFromHumanReadable "011101000000110"
typeInformation Q Mask4 = binaryFromHumanReadable "010010010110100"
typeInformation Q Mask5 = binaryFromHumanReadable "010000110000011"
typeInformation Q Mask6 = binaryFromHumanReadable "010111011011010"
typeInformation Q Mask7 = binaryFromHumanReadable "010101111101101"

binaryFromHumanReadable :: [Char] -> [Bool]
binaryFromHumanReadable [] = []
binaryFromHumanReadable ('1':xs) = True:binaryFromHumanReadable xs
binaryFromHumanReadable ('0':xs) = False:binaryFromHumanReadable xs

formatPath1 = [(0, 8), (1, 8), (2, 8), (3, 8), (4, 8), (5, 8), (7, 8), (8, 8), (8, 7), (8,5), (8,4), (8,3),(8,2),(8,1),(8,0)]

formatPath2 = [(8,20),(8,19),(8,18),(8,17),(8,16),(8,15),(8,14),(13,8),(14,8),(15,8),(16,8),(17,8),(18,8),(19,8),(20,8)]

upwardColumn = cycle [(-1,0),(1,-1)]
downwardColumn = cycle [(-1,0),(1,1)]

pathFrom end [] = [end]
pathFrom (x,y) ((dx,dy):deltas) = (x,y) : pathFrom (x+dx,y+dy) deltas

dataPattern = pathFrom (20, 20) $ (take 23 upwardColumn) ++ [(-1, 0)] ++ (take 23 downwardColumn) ++ [(-1,0)]  ++ (take 23 upwardColumn) ++ [(-1,0)] ++ (take 23 downwardColumn) ++ [(-1,0)] ++ (take 27 upwardColumn) ++ [(1,-2)] ++ (take 11 upwardColumn) ++ [(-1,0)] ++ (take 11 downwardColumn) ++ [(1,2)] ++ (take 27 downwardColumn) ++ [(-1,-8)] ++ (take 7 upwardColumn) ++ [(-2,0)] ++ (take 7 downwardColumn) ++ [(-1,0)] ++ (take 7 upwardColumn) ++ [(-1,0)] ++ (take 7 downwardColumn)

--- MASK TESTING ---

minX :: Bitmap2D -> Int
minX = (fst . fst) . bounds

maxX :: Bitmap2D -> Int
maxX = (fst . snd) . bounds

minY :: Bitmap2D -> Int
minY = (snd . fst) . bounds

maxY :: Bitmap2D -> Int
maxY = (snd . snd) . bounds

rows :: Bitmap2D -> [[Bool]]
rows bitmap = [(map ((!) bitmap) (indicesForRow r)) | r <- [((snd . fst) (bounds bitmap))..((snd . snd) (bounds bitmap))]] 
    where indicesForRow r   = range ( ( (fst . fst) (bounds bitmap), r), ( (fst . snd) (bounds bitmap), r ) )

columns :: Bitmap2D -> [[Bool]]
columns bitmap = [(map ((!) bitmap) (indicesForCol c)) | c <- [((fst . fst) (bounds bitmap))..((fst . snd) (bounds bitmap))]]
    where indicesForCol c   = range ( (c, (snd . fst) (bounds bitmap)), (c, (snd . snd) (bounds bitmap) ) )

penaltyRule1 :: Bitmap2D -> Int
penaltyRule1 bitmap = horizontalScore + verticalScore
    where horizontalScore   = sum (map scoreSequence (rows bitmap))
          verticalScore     = sum (map scoreSequence (columns bitmap))

scoreSequence :: [Bool] -> Int
scoreSequence [] = 0
scoreSequence xs = runScore + scoreSequence (drop runLength xs)
    where leader    = head xs
          runLength = length $ takeWhile (== leader) xs
          runScore  = if runLength >= 5 then runLength-2 else 0

penaltyRule2 :: Bitmap2D -> Int
penaltyRule2 bitmap = length (filter sameColor blocks) * 3
    where blocks       = map (\shape -> map ((!) bitmap) shape) blockShapes
          sameColor xs = length (takeWhile (== (head xs)) xs) == length xs
          blockShapes  = [ range ((x,y),(x+1,y+1)) | x <- [minX bitmap..maxX bitmap - 1], y <- [minY bitmap..maxY bitmap - 1]]

pr3 :: [Bool] -> Int
pr3 list
    | length list < 11 = 0
    | isPrefixOf [False, False, False, False, True, False, True, True, True, False, True, False, False, False, False] list = 1 + pr3 (drop 11 list)
    | isPrefixOf [True, False, True, True, True, False, True, False, False, False, False] list = 1 + pr3 (drop 7 list)
    | isPrefixOf [False, False, False, False, True, False, True, True, True, False, True] list = 1 + pr3 (drop 7 list)
    | otherwise = pr3 (drop 1 list)

penaltyRule3 :: Bitmap2D -> Int
penaltyRule3 bitmap = 40 * (horizontalScore + verticalScore)
    where horizontalScore = sum (map pr3 (rows bitmap))
          verticalScore   = sum (map pr3 (columns bitmap))

penaltyRule4 :: Bitmap2D -> Int
penaltyRule4 bitmap = truncate step4
    where darkPixels   = length $ filter id pixels
          totalPixels  = length pixels
          pixels       = elems bitmap
          step1        = (fromIntegral darkPixels) / (fromIntegral totalPixels)
          step2        = (step1 * 100) - 50
          step3        = abs (truncate step2)
          step4        = ((fromIntegral step3) / 5) * 10

penalty :: Bitmap2D -> Int
penalty b = sum (map (\rule -> rule b) [penaltyRule1, penaltyRule2, penaltyRule3, penaltyRule4])
