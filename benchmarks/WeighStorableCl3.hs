
import Weigh
import Data.Vector.Storable as V
import Data.Massiv.Array as A
import Algebra.Geometric.Cl3

main :: IO ()
main = mainWith $ do func' "Standard Cl3 Real Number in Vector" apsR unitR
                     func' "Compact Cl3 Real Number in Vector" compactR unitR
                     func' "Standard Cl3 Real Number in Massiv Vector" apsRMassiv unitR
                     func' "Compact Cl3 Real Number in Massiv Vector" compactRMassiv unitR

apsR :: Cl3 -> V.Vector Cl3
apsR cl3 = V.replicate 50000 cl3

compactR :: Cl3 -> V.Vector Cl3_R
compactR cl3 = V.replicate 50000 (toCl3_R cl3)

apsRMassiv :: Cl3 -> A.Array A.S A.Ix1 Cl3
apsRMassiv cl3 = A.makeVectorR A.S A.Seq (A.Sz1 50000) (const cl3)

compactRMassiv :: Cl3 -> A.Array A.S A.Ix1 Cl3_R
compactRMassiv cl3 = A.makeVectorR A.S A.Seq (A.Sz1 50000) (const $ toCl3_R cl3)

unitR :: Cl3
unitR = R 1
