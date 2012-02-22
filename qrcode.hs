import Graphics.Rendering.Cairo
import Debug.Trace
import Char

import QREncode

-- implements Version 1 QR Code

main = do
    withImageSurface FormatRGB24 210 210 render

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

        -- format information
        -- H EC level (all white)
        --
        -- mask pattern i%2=0 (horizontal stripes)
        rectangle 2 8 1 1
        fill

        rectangle 8 18 1 1
        fill

        -- format error correction
        -- TODO

        -- fixed patterns
        rectangle 6 8 1 1
        rectangle 6 10 1 1
        rectangle 6 12 1 1

        rectangle 8 6 1 1
        rectangle 10 6 1 1
        rectangle 12 6 1 1

        rectangle 8 13 1 1
        fill
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

