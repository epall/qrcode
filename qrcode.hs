import Debug.Trace
import Graphics.Rendering.Cairo

import QREncode

-- implements Version 1 QR Code

main = do
    withImageSurface FormatRGB24 210 210 render

typeInformation = [False, True, True, False, True, False, True, False, True, False, True, True, True, True, True]

formatPath1 = [(0, 8), (1, 8), (2, 8), (3, 8), (4, 8), (5, 8), (7, 8), (8, 8), (8, 7), (8,5), (8,4), (8,3),(8,2),(8,1),(8,0)]

formatPath2 = [(8,20),(8,19),(8,18),(8,17),(8,16),(8,15),(8,14),(13,8),(14,8),(15,8),(16,8),(17,8),(18,8),(19,8),(20,8)]

dataBits = stringWithErrorCorrection 1 Q "HELLO WORLD"

upwardColumn = cycle [(-1,0),(1,-1)]
downwardColumn = cycle [(-1,0),(1,1)]

pathFrom end [] = [end]
pathFrom (x,y) ((dx,dy):deltas) = (x,y) : pathFrom (x+dx,y+dy) deltas

dataPattern = pathFrom (20, 20) $ (take 23 upwardColumn) ++ [(-1, 0)] ++ (take 23 downwardColumn) ++ [(-1,0)]  ++ (take 23 upwardColumn) ++ [(-1,0)] ++ (take 23 downwardColumn) ++ [(-1,0)] ++ (take 27 upwardColumn) ++ [(1,-2)] ++ (take 11 upwardColumn) ++ [(-1,0)] ++ (take 11 downwardColumn) ++ [(1,2)] ++ (take 27 downwardColumn)

data Mask = MaskNone | Mask0

render :: Surface -> IO ()
render surface = do
    renderWith surface (do
        scale 10 10
        -- blank to white
        setSourceRGB 1.0 1.0 1.0
        rectangle 0 0 21 21
        fill

        -- alignment boxes
        alignmentBox 0 0
        alignmentBox 14 0
        alignmentBox 0 14

        -- fixed patterns
        rectangle 6 8 1 1
        rectangle 6 10 1 1
        rectangle 6 12 1 1

        rectangle 8 6 1 1
        rectangle 10 6 1 1
        rectangle 12 6 1 1

        rectangle 8 13 1 1
        fill

        -- format information
        drawPixelsOnPath MaskNone typeInformation formatPath1
        drawPixelsOnPath MaskNone typeInformation formatPath2
        fill

        -- data
        drawPixelsOnPath Mask0 dataBits dataPattern
        )
    surfaceWriteToPNG surface "qr.png"

alignmentBox :: Double -> Double -> Render ()
alignmentBox x y = do
    currentMatrix <- getMatrix
    translate x y
    setSourceRGB 0 0 0

    rectangle 0 0 7 1
    rectangle 0 0 1 7
    rectangle 6 0 1 7
    rectangle 0 6 7 1

    rectangle 2 2 3 3
    fill
    setMatrix currentMatrix

pixel :: Int -> Int -> Render ()
pixel x y = do
    rectangle (fromIntegral x) (fromIntegral y) 1 1
    fill


drawPixelsOnPath :: Mask -> [Bool] -> [(Int, Int)] -> Render ()
drawPixelsOnPath _ [] [] = return ()
drawPixelsOnPath _ _ [] = return () --fail "ran out of path"
drawPixelsOnPath _ [] _ = fail "ran out of pixels"

drawPixelsOnPath mask (bit:pixels) ((x,y):path) = do
    setSourceRGB 0 1 1
    moveTo x y

    setSourceRGB 0 0 0
    if applyMask mask x y bit
        then pixel x y
        else return ()
    drawPixelsOnPath mask pixels path

applyMask :: Mask -> Int -> Int -> Bool -> Bool

applyMask MaskNone x y bit = bit
applyMask Mask0 x y bit
    | (y + x) `mod` 2 == 0 = not bit
    | otherwise = bit
