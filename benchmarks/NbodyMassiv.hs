{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}



import Algebra.Geometric.Cl3
import Data.Massiv.Array as A
import Control.Monad (replicateM_,when)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))

import Data.IORef (IORef(),newIORef,modifyIORef',readIORef)


main :: IO ()
main = do
  arrayProgrammingAlgorithm
  bitTwiddlingAlgorithm
  
arrayProgrammingAlgorithm :: IO ()
arrayProgrammingAlgorithm = 
  do tStart <- getPOSIXTime
     n <- return (50000000 :: Int)
     planetsPos :: Array S Ix1 Cl3_V3 <- fromListsM Seq planetspos  -- generate immutible Massiv array of the planets position
     planetsVel :: Array S Ix1 Cl3_V3 <- fromListsM Seq planetsvel  -- generate immutible Massiv array of the planets velocity
     planetsMass :: Array S Ix1 Cl3_R <- fromListsM Seq planetsmass  -- generate immutible Massiv array of the planets mass
     mutablePos <- thaw planetsPos  -- copy to a mutible Massiv array of the planets position
     mutableVel <- thaw planetsVel  -- copy to a mutible Massiv array of the planets velocity
     let (Sz1 c) = size planetsVel  -- Size of the velocity array
         c' = (c^(2 :: Int) - c) `div` 2  -- size of the nChoose2With array
     mutableScratch <- newMArray (Sz1 c') (toCl3_V3 $ V3 0 0 0) -- nChoose2With sized scratch pad
     mutableScratch2D <- newMArray (Sz (c :. (c-1))) (toCl3_V3 $ V3 0 0 0) -- 2D scratch pad for holding acceleration components
     offsetMomentum mutableVel planetsMass -- modify the Sun's velocity such that the total momentum is zero
     printEnergy mutablePos mutableVel planetsMass
     replicateM_ n (advance mutablePos mutableVel planetsMass mutableScratch mutableScratch2D)
     printEnergy mutablePos mutableVel planetsMass
     tEnd <- getPOSIXTime
     print $ show (tEnd - tStart) ++ " (sec) for " ++ show n ++ " iterations." -- Relative time measurement

-- | 'bitTwiddlingAlgorithm' with Ϟ symbolizing the bit twiddling algorithm versions
bitTwiddlingAlgorithm :: IO ()
bitTwiddlingAlgorithm = 
  do tStart <- getPOSIXTime
     n <- return (50000000 :: Int)
     (solarSystem,sz) <- offsetMomentumϞ [sun,jupiter,saturn,uranus,neptune] -- returns a zero momentum solar system
     printEnergyϞ solarSystem sz
     replicateM_ n (advanceϞ solarSystem sz)
     printEnergyϞ solarSystem sz
     tEnd <- getPOSIXTime
     print $ show (tEnd - tStart) ++ " (sec) for " ++ show n ++ " iterations." -- Relative time measurement

offsetMomentumϞ :: [Body] -> IO (SolarSystem,Int) -- A SolarSystem and the Size : size: sz :: Int
offsetMomentumϞ bodies = do
  initBodies :: Array S Ix1 Body <- A.fromListsM Seq bodies
  -- mutate the solar system so that the sun's velocity gives zero total momentum
  let velocity :: Array D Ix1 Cl3 = A.map (fromCl3_V3.vel) initBodies
      mass :: Array D Ix1 Cl3 = A.map (fromCl3_R.mas) initBodies
  ss :: SolarSystem <- thaw initBodies
  let sz = totalElem.sizeOfMArray $ ss
  bs <- readM ss 0
  let sunV = fromCl3_V3.vel $ bs
      sunM = fromCl3_R.mas $ bs
  writeM ss 0 (bs{vel = toCl3_V3 (sunV - ((velocity `dot_V3` mass) / sunM))})
  return (ss,sz)


printEnergyϞ :: SolarSystem -> Int -> IO ()
printEnergyϞ ss sz = do
  acc :: IORef Cl3 <- newIORef (R 0)
  ssFroz <- freeze Seq ss
  iforIO_ ssFroz $ \idx bi ->
    do let bi_pos :: Cl3 = fromCl3_V3.pos $ bi
           bi_vel :: Cl3 = fromCl3_V3.vel $ bi
           bi_mas :: Cl3 = fromCl3_R.mas $ bi
           ke :: Cl3 = 0.5 * bi_mas * (toR $ bi_vel^(2::Int))
       modifyIORef' acc (+ ke)  -- accumulate the kenetic energy for each body
       -- now calcualate the potential energy
       let szMinus1 = sz-1
       when (idx < szMinus1) $ do
         brest <- extractM (idx+1) (Sz1 (szMinus1-idx)) ssFroz
         forIO_ brest $ \bj ->
           do let bj_pos :: Cl3 = fromCl3_V3.pos $ bj
                  bj_mas :: Cl3 = fromCl3_R.mas $ bj
                  distance :: Cl3 = sqrt.toR $ (bi_pos - bj_pos)^(2::Int)
                  pe_ij :: Cl3 = (bi_mas * bj_mas) / distance
              modifyIORef' acc (\ac -> ac - pe_ij)
  energy <- readIORef acc
  print energy

advanceϞ :: SolarSystem -> Int -> IO ()
advanceϞ ss sz = do
  updateVelocityϞ ss sz
  updatePositionϞ ss

updateVelocityϞ :: SolarSystem -> Int -> IO ()
updateVelocityϞ ss sz = do
  ssFroz <- freeze Seq ss
  iforIO_ ssFroz $ \idx bi ->
    when (idx < sz-1) $ do
      let bi_pos :: Cl3 = fromCl3_V3.pos $ bi
          bi_mas :: Cl3 = fromCl3_R.mas $ bi
      brest <- extractM (idx+1) (Sz1 (sz-idx-1)) ssFroz
      iforIO_ brest $ \jdx bj ->
        do let bj_pos :: Cl3 = fromCl3_V3.pos $ bj
               bj_mas :: Cl3 = fromCl3_R.mas $ bj
               (absMag,dposUnit) = abssignum $ bi_pos - bj_pos
               mag = dt / ((absMag)^(2::Int))
               aij = negate $ dposUnit * bj_mas * mag
               aji = dposUnit * bi_mas * mag
           modify_ ss (modVel aij) idx
           modify_ ss (modVel aji) (idx+jdx+1)

modVel :: PrimMonad m => Cl3 -> Body -> m Body
modVel a bod = do
  let vbod = fromCl3_V3 $ vel bod
  return bod{vel = toCl3_V3 $ vbod + a}

updatePositionϞ :: SolarSystem -> IO ()
updatePositionϞ ss = do
  iforPrimM_ ss $ \idx b ->
    do let p :: Cl3 = fromCl3_V3.pos $ b
           v :: Cl3 = fromCl3_V3.vel $ b
       writeM ss idx b{pos = toCl3_V3 (p + dt * v)}
        

advance :: MArray RealWorld S Ix1 Cl3_V3 -> MArray RealWorld S Ix1 Cl3_V3 -> Array S Ix1 Cl3_R -> MArray RealWorld S Ix1 Cl3_V3 -> MArray RealWorld S Ix2 Cl3_V3 -> IO ()
advance sysPos sysVel sysMass scratchPad scratchPad2D =
  do updateVelocity sysPos sysVel sysMass scratchPad scratchPad2D
     updatePosition sysPos sysVel

updateVelocity :: MArray RealWorld S Ix1 Cl3_V3 -> MArray RealWorld S Ix1 Cl3_V3 -> Array S Ix1 Cl3_R -> MArray RealWorld S Ix1 Cl3_V3 -> MArray RealWorld S Ix2 Cl3_V3 -> IO ()
updateVelocity sysPos sysVel sysMass scratchPad scratchPad2D =
  do -- Calculate the inverse square distance and unit vector between the bodies
     frozPos <- freeze Seq sysPos
     let ps :: Array D Ix1 Cl3 = A.map fromCl3_V3 frozPos
         scaledPij = nChoose2With (\ i j -> let (a,s) = abssignum $ i - j in (recip (a^(2 :: Int))) * s) ps -- probably should use a computeSource here, because scaledPij is used twice, or verify streams are still working.
     computeInto scratchPad (A.map toCl3_V3 scaledPij)  -- Compute result to the scratch pad
     frozScratch <- freeze Seq scratchPad  -- Freeze the result for future use
     -- Scale the previous inverse square distance by the mass of the other body
     let scratch :: Array D Ix1 Cl3 = A.map fromCl3_V3 frozScratch
         ms :: Array D Ix1 Cl3 = A.map fromCl3_R sysMass
         upper = nChoose2With (\ _ j -> negate j) ms !*! scratch
         lower = nChoose2With (\ i _ -> i) ms !*! scratch
     computeInto scratchPad (A.map toCl3_V3 upper)
     frozScratchUpper <- freeze Seq scratchPad
     computeInto scratchPad (A.map toCl3_V3 lower)
     frozScratchLower <- freeze Seq scratchPad
     -- Load each body acceleration contribution into a 2D array 
     frozVel <- freeze Seq sysVel
     let vs :: Array D Ix1 Cl3 = A.map fromCl3_V3 frozVel
         rows = elemsCount vs
     iforPrimM_ scratchPad2D (\ix _ -> write_ scratchPad2D ix (genAcc rows frozScratchUpper frozScratchLower ix))
     frozScratch2D <- freeze Seq scratchPad2D
     --  Sum each row to calcuate the total acceleration and then update the velocity
     let accComponents :: Array D Ix2 Cl3 = A.map fromCl3_V3 frozScratch2D
         as = makeArray Seq (size vs) (\ ix -> foldlS (+) (V3 0 0 0) (accComponents !> ix))
         vs' = vs !+! (dt *. as)
     computeInto sysVel (A.map toCl3_V3 vs')

genAcc :: Int -> Array S Ix1 Cl3_V3 -> Array S Ix1 Cl3_V3 -> Ix2 -> Cl3_V3
genAcc rows upper lower (row :. col) | row <= col = upper ! unSplitIdx rows (row,col+1)
                                     | otherwise  = lower ! unSplitIdx rows (col,row)

unSplitIdx :: Int -> (Int,Int) -> Int
unSplitIdx c (row,col) = go 0 (c-1) 0 + (col-1 - row)
  where
    go n c1 acc | n == row = acc
                | otherwise = go (n+1) (c1-1) (acc+c1)



updatePosition :: MArray RealWorld S Ix1 Cl3_V3 -> MArray RealWorld S Ix1 Cl3_V3 -> IO ()
updatePosition sysPos sysVel =
  do frozPos <- freeze Seq sysPos
     frozVel <- freeze Seq sysVel
     let ps :: Array D Ix1 Cl3 = A.map fromCl3_V3 frozPos
         vs :: Array D Ix1 Cl3 = A.map fromCl3_V3 frozVel
         ps' = ps !+! (dt *. vs)
     computeInto sysPos (A.map toCl3_V3 ps')


offsetMomentum :: MArray RealWorld S Ix1 Cl3_V3 -> Array S Ix1 Cl3_R -> IO ()
offsetMomentum sysVel sysMass =
  do frozVel <- freeze Seq sysVel
     planetsvs :: Array D Ix1 Cl3 <- extractM 1 4 (A.map fromCl3_V3 frozVel)  -- select just the planets and not the sun
     planetms :: Array D Ix1 Cl3 <- extractM 1 4 (A.map fromCl3_R sysMass)  -- select just the planets and not the sun
     (fromCl3_R -> sunMassCl3) <- sysMass !? 0
     writeM sysVel 0 (toCl3_V3 $ negate (planetsvs `dot_V3` planetms) / sunMassCl3)
     

-- dot_V3 is to work around the typical sum in the dot product that has an aculumulator of a Real 0 value
dot_V3 :: Array D Ix1 Cl3 -> Array D Ix1 Cl3 -> Cl3
dot_V3 v1 v2 = foldlS (+) (V3 0 0 0) (A.zipWith (*) v1 v2)


printEnergy :: MArray RealWorld S Ix1 Cl3_V3 -> MArray RealWorld S Ix1 Cl3_V3 -> Array S Ix1 Cl3_R -> IO ()
printEnergy sysPos sysVel sysMass =
  do frozPos <- freeze Seq sysPos
     frozVel <- freeze Seq sysVel
     let ps :: Array D Ix1 Cl3 = A.map fromCl3_V3 frozPos
         vs :: Array D Ix1 Cl3 = A.map fromCl3_V3 frozVel
         ms :: Array D Ix1 Cl3 = A.map fromCl3_R sysMass
         ke = 0.5 * (ms !.! A.map toR (vs.^2)) -- Kenetic Energy O(n)
         pe = negate.A.sum $ nChoose2With (*) ms !/! nChoose2With (\ x y -> abs $ x - y) ps -- Gravitational Potential Energy O((n^2 - n)/2)
     print $ ke + pe -- print the total energy

-- This function is a bit of a combinatorial explosion so it is marked to be evaluated in Seqallel, probably should convert to a sgenerate :: Sz1 -> (Ix1 -> e) -> Vector DS e 
nChoose2With :: (Cl3 -> Cl3 -> Cl3) -> Array D Ix1 Cl3 -> Array D Ix1 Cl3
nChoose2With f bs =
  let (Sz1 c) = size bs
      c' = (c^(2 :: Int) - c) `div` 2
      bs' :: Array BN Ix1 Cl3 = computeSource bs
  in makeArray Seq (Sz1 c') (genElements f bs' c)

genElements :: (Cl3 -> Cl3 -> Cl3) -> Array BN Ix1 Cl3 -> Int -> Ix1 -> Cl3
genElements f bs c idx =
  let (ix1,ix2) = splitIdx c idx
  in f (bs ! ix1) (bs ! ix2)

splitIdx :: Int -> Int -> (Int,Int)
splitIdx c = go (0,1) (c-2)
  where
    go (row,col) x ix | ix - x <= 0 = (row, col + ix)
                      | otherwise = go (row+1,col+1) (x-1) (ix-x-1)

planetspos :: [Cl3_V3]
planetspos = pos <$> [sun,jupiter,saturn,uranus,neptune]

planetsvel :: [Cl3_V3]
planetsvel = vel <$> [sun,jupiter,saturn,uranus,neptune]

planetsmass :: [Cl3_R]
planetsmass = mas <$> [sun,jupiter,saturn,uranus,neptune]

type SolarSystem = MArray RealWorld S Ix1 Body

data Body = Body
  {pos :: Cl3_V3
  ,vel :: Cl3_V3
  ,mas :: Cl3_R}

instance Storable Body where
  sizeOf _ = sizeOf (undefined :: Cl3_V3) + sizeOf (undefined :: Cl3_V3) + sizeOf (undefined :: Cl3_R)
  alignment _ = sizeOf (undefined :: Double) -- alignment must be a 2^x
  peekElemOff ptr idx = peek (castPtr ptr `plusPtr` (idx * sizeOf (undefined :: Body)))
  pokeElemOff ptr idx e = poke (castPtr ptr `plusPtr` (idx * sizeOf (undefined :: Body))) e
  peek :: Ptr Body -> IO Body
  peek ptr = do
    p :: Cl3_V3 <- peek (castPtr ptr `plusPtr` 0)
    v :: Cl3_V3 <- peek (castPtr ptr `plusPtr` (sizeOf p))
    m :: Cl3_R <- peek (castPtr ptr `plusPtr` (sizeOf p) `plusPtr` (sizeOf v))
    return (Body p v m)
  poke :: Ptr Body -> Body -> IO ()
  poke ptr (Body p v m) = do
    poke (castPtr ptr `plusPtr` 0) p
    poke (castPtr ptr `plusPtr` (sizeOf p)) v
    poke (castPtr ptr `plusPtr` (sizeOf p) `plusPtr` (sizeOf v)) m

sun :: Body
sun = Body (toCl3_V3 $ V3 0 0 0) -- Sun Position
           (toCl3_V3 $ V3 0 0 0) -- Sun Velocity 
           (toCl3_R $ R solarMass) -- Sun Mass
jupiter :: Body
jupiter = Body (toCl3_V3 $ V3 (4.84143144246472090e+00) (-1.16032004402742839e+00) (-1.03622044471123109e-01))
               (toCl3_V3 $ V3 (1.66007664274403694e-03*dp) (7.69901118419740425e-03*dp) (-6.90460016972063023e-05*dp))
               (toCl3_R $ R (9.54791938424326609e-04 * solarMass))
saturn :: Body
saturn = Body (toCl3_V3 $ V3 (8.34336671824457987e+00) (4.12479856412430479e+00) (-4.03523417114321381e-01))
              (toCl3_V3 $ V3 (-2.76742510726862411e-03*dp) (4.99852801234917238e-03*dp) (2.30417297573763929e-05*dp))
              (toCl3_R $ R (2.85885980666130812e-04 * solarMass))
uranus :: Body
uranus = Body (toCl3_V3 $ V3 (1.28943695621391310e+01) (-1.51111514016986312e+01) (-2.23307578892655734e-01))
              (toCl3_V3 $ V3 (2.96460137564761618e-03*dp) (2.37847173959480950e-03*dp) (-2.96589568540237556e-05*dp))
              (toCl3_R $ R (4.36624404335156298e-05 * solarMass))
neptune :: Body
neptune = Body (toCl3_V3 $ V3 (1.53796971148509165e+01) (-2.59193146099879641e+01) (1.79258772950371181e-01))
               (toCl3_V3 $ V3 (2.68067772490389322e-03*dp) (1.62824170038242295e-03*dp) (-9.51592254519715870e-05*dp))
               (toCl3_R $ R (5.15138902046611451e-05 * solarMass))


daysPerYear :: Double
daysPerYear = 365.24

solarMass :: Double
solarMass = 4 * pi^(2 :: Int)
dp :: Double
dp = daysPerYear
dt :: Cl3
dt = R 0.01
