
import Weigh
import Data.Vector.Storable as V
import Data.Massiv.Array as A
import Algebra.Geometric.Cl3
import Foreign.Ptr (Ptr(), plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (mallocBytes)

main :: IO ()
main = mainWith $ do func' "Cl3.R in Vector" apsR unitR
                     func' "Cl3.Cl3_R in Vector" compactR unitR
                     func' "Cl3.R in Massiv Vector" apsRMassiv unitR
                     func' "Cl3.Cl3_R in Massiv Vector" compactRMassiv unitR
                     action "malloc Storable Cl3_R by (1024*1024)" $ mallocR unitR

apsR :: Cl3 -> V.Vector Cl3
apsR cl3 = V.replicate (1024*1204) cl3

compactR :: Cl3 -> V.Vector Cl3_R
compactR cl3 = V.replicate (1024*1204) (toCl3_R cl3)

apsRMassiv :: Cl3 -> A.Array A.S A.Ix1 Cl3
apsRMassiv cl3 = A.makeVectorR A.S A.Seq (A.Sz1 (1024*1204)) (const cl3)

compactRMassiv :: Cl3 -> A.Array A.S A.Ix1 Cl3_R
compactRMassiv cl3 = A.makeVectorR A.S A.Seq (A.Sz1 (1024*1204)) (const $ toCl3_R cl3)

mallocR :: Cl3 -> IO (Ptr Cl3_R)
mallocR cl3 = do
  pa <- mallocBytes ((1024*1204) * sizeOf (undefined :: Cl3_R))
  let fill idx | idx < (1024*1204) = do
        poke (pa `plusPtr` (idx * sizeOf (undefined :: Cl3_R))) cl3
        fill (idx+1)
      fill _ | otherwise = return ()
  fill 0
  peekElemOff pa (1024*1204)

unitR :: Cl3
unitR = R 1
