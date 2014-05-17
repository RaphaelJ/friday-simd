{-# LANGUAGE BangPatterns, FlexibleContexts #-}
import Criterion.Main
import Data.Int

import Vision.Image (GreyImage)
import qualified Vision.Image as I
import Vision.Histogram (Histogram)
import qualified Vision.Histogram.Simd as HS
import Vision.Primitive

path :: FilePath
path = "bench/image.jpg"

main :: IO ()
main = do
    Right io <- I.load path Nothing
    let !grey          = I.convert io             :: GreyImage
        !hist          = H.histogram grey Nothing :: Histogram DIM1 Int32

    defaultMain [
          bgroup "SIMD histograms" [
              bench "intersection comparison" $
                whnf (HS.compareIntersectInt32 hist) hist
            ]
        ]
