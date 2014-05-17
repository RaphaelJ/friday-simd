module Vision.Histogram.Simd (compareIntersectInt32) where

import Data.Int
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe
import Vision.Histogram (Histogram (..))
import Vision.Primitive (Shape)

foreign import ccall unsafe "compare_intersect_int" compare_intersect_int ::
    Ptr Int32 -> Ptr Int32 -> CSize -> IO CInt

compareIntersectInt32 :: Shape sh => Histogram sh Int32 -> Histogram sh Int32
                     -> Int32
compareIntersectInt32 (Histogram sh1 vec1) (Histogram sh2 vec2)
    | sh1 /= sh2 = error "Histograms are not of equal size."
    | otherwise  = int32 $! unsafePerformIO $
        V.unsafeWith vec1 $ \ptr1 ->
            V.unsafeWith vec2 $ \ptr2 ->
                compare_intersect_int (castPtr ptr1) (castPtr ptr2)
                                      (fromIntegral (V.length vec1))

int32 :: Integral a => a -> Int32
int32 = fromIntegral
