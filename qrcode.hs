import Data.Array
import Control.Applicative
import Graphics.Rendering.Cairo

import QRShared
import QREncode
import QRDraw

main =
    let
        dataBits = stringWithErrorCorrection 1 Q "HELLO WORLD"
        bitmap = dataToBitmapWithBestMask dataBits Q
        render = renderBitmap bitmap
    in do
        withImageSurface FormatRGB24 210 210 render

renderBitmap :: Bitmap2D -> Surface -> IO ()
renderBitmap bitmap surface = 
    let pixels = assocs bitmap
        blackPixels = (filter (\(_,val) -> val) pixels)
        drawPixels = map (\((x,y),_) -> rectangle (fromIntegral x) (fromIntegral y) 1 1) blackPixels
    in do
        renderWith surface (do
            scale 10 10
            setSourceRGB 1 1 1
            rectangle 0 0 21 21
            fill
            setSourceRGB 0 0 0
            sequence_ drawPixels
            fill
            )
        surfaceWriteToPNG surface "qr.png"
