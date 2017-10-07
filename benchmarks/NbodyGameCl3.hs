{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

------------------------------------------------------------------
-- | 
-- Description: Benchmark for Algebra.Geometric.Cl3
-- Uses standard "The Computer language Benchmarks Game" implementation
-- with Criterion to compare the performace difference between
-- using the Cl3 library vs. the benchmarks implementation using doubles.
-- 
-- 
-- 
-- This derivative work was derived from the exelent work of:
-- Branimir Maksimovic.
-- 
-------------------------------------------------------------------

module Main (main) where

import Criterion.Main (defaultMain, bgroup, bench, nfIO)  -- To add Criternion to the benchmark
import Algebra.Geometric.Cl3 (Cl3(..), toR, toV3) -- To add Cl3
import Algebra.Geometric.Cl3.Storable()  -- To add Storable Cl3

import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (mallocBytes)
import Control.Monad (when)
import Text.Printf (printf)

-------------------------------------------------------------------
-- |
-- An n-body solver for the Sun and gas giants using the semi-implicit Euler method.
-- The benchmark uses the Criterion tool to measure timing.
-- The benchmark advances 50 million steps.  
-- 
-- The @nbodyBaseline@ is derived from Branimir Maksimovic's work with some changes suggested by @hlint@.
-- @nbodyCl3@ agressivly uses the "to" functions like 'toR' and 'toV3' to take advantage of the case of
-- known constructor optimisation.  @nbodyAPS@ does not make agressive use "to" functions.  The
-- program would not finish compiling (memory would grow to 54GB while compiling) before INLINE pragmas 
-- were added to the Cl3 library.
-- 
-- >>> ./benchmarks/NbodyGameCl3
-- benchmarking nbodyBaseline/50000000
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- -0.169075164
-- -0.169059907
-- time                 37.99 s    (37.92 s .. 38.08 s)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 37.93 s    (37.89 s .. 37.95 s)
-- std dev              36.25 ms   (0.0 s .. 38.29 ms)
-- variance introduced by outliers: 19% (moderately inflated)
-- benchmarking nbodyCl3/50000000
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- R (-0.16907516382852447)
-- R (-0.16905990680272506)
-- time                 38.95 s    (38.91 s .. 39.03 s)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 38.95 s    (38.94 s .. 38.96 s)
-- std dev              14.70 ms   (0.0 s .. 16.66 ms)
-- variance introduced by outliers: 19% (moderately inflated)
-- benchmarking nbodyAPS/50000000
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- R (-0.16907516382852447)
-- R (-0.16905990678998767)
-- time                 179.4 s    (177.4 s .. 181.4 s)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 179.3 s    (178.8 s .. 179.5 s)
-- std dev              395.0 ms   (0.0 s .. 413.9 ms)
-- variance introduced by outliers: 19% (moderately inflated)
--
-- 
------------------------------------------------------------------- 

main :: IO ()
main = do n <- return 50000000
          defaultMain [bgroup "nbodyBaseline" [ bench (show n) $ nfIO (nbodyBaseline n)],
                       bgroup "nbodyCl3" [ bench (show n) $ nfIO (nbodyCl3 n)],
                       bgroup "nbodyAPS" [ bench (show n) $ nfIO (nbodyAPS n)]
                       ]



nbodyBaseline :: (Num t, Eq t) => t -> IO ()
nbodyBaseline n = do
  pPlanets <- fromList planets          -- load planets into a stack
  nbodyInit pPlanets                    -- initialize the system momentum
  energy pPlanets >>= printf "%.9f\n"   -- calculate and print out the initial energy of the system
  run n pPlanets                        -- solve the initial value problem, incriment the system 'n' times
  energy pPlanets >>= printf "%.9f\n"   -- calculate and print out the final energy of the system


nbodyCl3 :: (Eq t, Num t) => t -> IO ()
nbodyCl3 n = do
  pPlanetsCl3 <- fromListCl3 planetsCl3
  nbodyInitCl3 pPlanetsCl3
  energyCl3 pPlanetsCl3 >>= print.toR -- printf "%0.9f\n"
  runCl3 n pPlanetsCl3
  energyCl3 pPlanetsCl3 >>= print.toR -- printf "%0.9f\n"


nbodyAPS :: (Eq t, Num t) => t -> IO ()
nbodyAPS n = do
  pPlanetsAPS <- fromListCl3 planetsCl3
  nbodyInitAPS pPlanetsAPS
  energyAPS pPlanetsAPS >>= print.toR -- printf "%0.9f\n"
  runAPS n pPlanetsAPS
  energyAPS pPlanetsAPS >>= print.toR -- printf "%0.9f\n"


-- | 'run' advances the system 'n' times by recursivly calling 'run'
run :: (Eq t, Num t) => t -> Ptr Planet -> IO ()
run 0 _ = return ()
run i p = do
  advance p
  run (i-1) p


runCl3 :: (Eq t, Num t) => t -> Ptr PlanetCl3 -> IO ()
runCl3 0 _ = return ()
runCl3 i p = do
  advanceCl3 p
  runCl3 (i-1) p


runAPS :: (Eq t, Num t) => t -> Ptr PlanetCl3 -> IO ()
runAPS 0 _ = return ()
runAPS i p = do
  advanceAPS p
  runAPS (i-1) p


-- | 'offsetMomentum' is used to update the sun's momentum
-- so that the total momentum will be zero for the whole
-- system at initialization.
offsetMomentum :: Planet -> (Double, Double, Double) -> Planet
offsetMomentum p (px,py,pz) = p {vx = -px / solarMass,
                                 vy = -py / solarMass,
                                 vz = -pz / solarMass
                                 }


offsetMomentumCl3 :: PlanetCl3 -> Cl3 -> PlanetCl3
offsetMomentumCl3 (PlanetCl3 (pos') _ (mass')) (momentum) = PlanetCl3 pos' (toV3.negate $ toV3 momentum * (toR.recip.toR $ mass')) mass'


offsetMomentumAPS :: PlanetCl3 -> Cl3 -> PlanetCl3
offsetMomentumAPS p momCl3 = p{velCl3 = negate momCl3 / massCl3 p}



-- | 'nbodyInit' initializes the velocity of the sun so that
-- the total system momentum is zero.
nbodyInit :: Ptr Planet -> IO ()
nbodyInit pPlanets = do
  let initBl (px,py,pz) i = -- do "Is this do do?"  -- init cycles through the stack and builds up the total momentum
        if i < length planets
        then do
            p <- peekElemOff pPlanets i
            initBl (px + vx p * mass p, py + vy p * mass p, pz + vz p * mass p) (i+1)  -- requires velocity and mass
        else return (px,py,pz)
  s <- initBl (0,0,0) 0                 -- total momentum of the system
  p <- peek pPlanets                  -- grab the sun, don't get burned
  poke pPlanets $ offsetMomentum p s  -- update the sun's momentum
-- end of nbodyInit


nbodyInitCl3 :: Ptr PlanetCl3 -> IO ()
nbodyInitCl3 pPlanetsCl3 = do
  let initCl3 (toV3 -> pCl3) i =
        if i < length planetsCl3
        then do
            (PlanetCl3 _ (toV3 -> vel') (toR -> mass')) <- peekElemOff pPlanetsCl3 i
            initCl3 (toV3 $! pCl3 + vel' * mass') (i+1)
        else return pCl3
  
  totalMomentum <- initCl3 (V3 0 0 0) 0
  sun <- peek pPlanetsCl3
  poke pPlanetsCl3 $ offsetMomentumCl3 sun totalMomentum
-- end of nbodyInitCl3


nbodyInitAPS :: Ptr PlanetCl3 -> IO ()
nbodyInitAPS pPlanetsCl3 = do
  let initCl3 !pCl3 i =
        if i < length planetsCl3
        then do
            p <- peekElemOff pPlanetsCl3 i
            initCl3 (pCl3 + velCl3 p * massCl3 p) (i+1)
        else return pCl3
  
  totalMomentum <- initCl3 (APS 0 0 0 0 0 0 0 0) 0
  sun <- peek pPlanetsCl3
  poke pPlanetsCl3 $ offsetMomentumAPS sun totalMomentum
-- end of nbodyInitAPS



-- | 'squared' scalar product of a vector with its self
squared :: Num a => a -> a -> a -> a
squared x' y' z' = x' * x' + y' * y' + z' * z'


-- | 'energy' calculate the total energy of the system
-- this is the gravitational potential energy added to 
-- the kenetic energy of all of the planets.
energy :: Ptr Planet -> IO Double
energy pPlanets = do
  let energy' e i = if i < length planets  -- ''
                    then do
                           p <- peekElemOff pPlanets i
                           e1 <- energy'' p (i+1) e
                           e2 <- energy' e (i+1)
                           return $ e + 0.5 * mass p * squared (vx p) (vy p) (vz p) + e1 + e2 -- requires mass and velocity
                    else return e
      energy'' p j e = if j < length planets
                       then do
                              pj <- peekElemOff pPlanets j
                              let distance = sqrt $ squared dx dy dz
                                  dx = x pj - x p
                                  dy = y pj - y p
                                  dz = z pj - z p
                              e1 <- energy'' p (j+1) e
                              return $ e - (mass p * mass pj) / distance + e1  -- requires mass and position
                       else return e
  energy' 0.0 0  -- starts off the recursive calcuation of energy


energyCl3 :: Ptr PlanetCl3 -> IO Cl3
energyCl3 pPlanetsCl3 = do
  let energy' e i = 
        if i < length planetsCl3
        then do
          p@(PlanetCl3 _ (toV3 -> vel1) (toR -> mass1)) <- peekElemOff pPlanetsCl3 i
          let !ke = toR $ 0.5 * mass1 * toR (vel1^2)
          e1 <- energy'' p (i+1) e
          e2 <- energy' e (i+1)
          return $! e + ke + e1 + e2
        else return e
      
      energy'' p@(PlanetCl3 (toV3 -> pos1) _ (toR -> mass1)) j e = 
        if j < length planetsCl3
        then do
          (PlanetCl3 (toV3 -> pos2) _ (toR -> mass2)) <- peekElemOff pPlanetsCl3 j
          let !dpos = toV3 $ pos2 - pos1
              !distance = abs dpos
              !pe = toR $ (mass1 * mass2) / distance
          e1 <- energy'' p (j+1) e
          return $! e - pe + e1  -- requires mass and position
        else return e
  
  energy' (R 0) 0  -- starts off the recursive calcuation of energy
-- end of energyCl3


energyAPS :: Ptr PlanetCl3 -> IO Cl3
energyAPS pPlanetsCl3 = do
  let energy' e i = 
        if i < length planetsCl3
        then do
          p <- peekElemOff pPlanetsCl3 i
          e1 <- energy'' p (i+1) e
          e2 <- energy' e (i+1)
          return $! e + 0.5 * massCl3 p * (velCl3 p)^2 + e1 + e2
        else return e
      
      energy'' p j e = 
        if j < length planetsCl3
        then do
          pj <- peekElemOff pPlanetsCl3 j
          let !distance = abs dpos
              !dpos = posCl3 pj - posCl3 p
          e1 <- energy'' p (j+1) e
          return $! e - (massCl3 p * massCl3 pj) / distance + e1
        else return e
  
  energy' (APS 0 0 0 0 0 0 0 0) 0  -- starts off the recursive calcuation of energy
-- end of energyAPS



-- | 'advance' integrates the system of differential equasions using a 
-- semi-implicit Euler method, also called symplectic Euler method.
-- The first order method updates the velocities based on the acceleration
-- calculation, then it updates the positions.  The velocity verlet
-- would be an improvement in accuracy because it is a second order
-- symplectic integrator.
advance :: Ptr Planet -> IO ()
advance pPlanets = do
  let advance' i = when (i < length planets) $ do  -- loops throught all the planets and updates the velocity
                     let loop j = when (j < length planets) $ do
                                    ii <- peekElemOff pPlanets i
                                    jj <- peekElemOff pPlanets j
                                    let mag = dt / (dSquared * sqrt dSquared)
                                        dSquared = squared dx dy dz
                                        dx = x ii - x jj
                                        dy = y ii - y jj
                                        dz = z ii - z jj
                                    pokeV pPlanets i ii{  -- requires position and mass to update the velocity
                                      vx = vx ii - dx * mass jj * mag,
                                      vy = vy ii - dy * mass jj * mag,
                                      vz = vz ii - dz * mass jj * mag
                                      }
                                    pokeV pPlanets j jj{  -- requires position and mass to update the velocity
                                      vx = vx jj + dx * mass ii * mag,
                                      vy = vy jj + dy * mass ii * mag,
                                      vz = vz jj + dz * mass ii * mag
                                      }
                                    loop (j+1)
                     loop (i+1)
                     advance' (i+1)
      advance'' i = when (i < length planets) $ do  -- loops through all of the planets and updates the position
                      p <- peekElemOff pPlanets i
                      pokeC pPlanets i p{  -- requires position and velocity to update the position
                        x = x p + dt * vx p,
                        y = y p + dt * vy p,
                        z = z p + dt * vz p
                        }
                      advance'' (i+1)
  advance' 0   -- update all of the planets velocities
  advance'' 0  -- update all of the planets positions
-- end of advance


advanceCl3 :: Ptr PlanetCl3 -> IO ()
advanceCl3 pPlanetsCl3 = do
  let advance' i = when (i < length planetsCl3) $ do  -- loops throught all the planets and updates the velocity
                     let loop j = when (j < length planetsCl3) $ do
                                    (PlanetCl3 (toV3 -> posi) (toV3 -> veli) (toR -> massi)) <- peekElemOff pPlanetsCl3 i
                                    (PlanetCl3 (toV3 -> posj) (toV3 -> velj) (toR -> massj)) <- peekElemOff pPlanetsCl3 j
                                    let !dpos = posi - posj
                                        !(R dMag) = abs dpos
                                        !mag = R (dt / (dMag^3))
                                        !veli' = veli - dpos * massj * mag
                                        !velj' = velj + dpos * massi * mag
                                    pokeVCl3 pPlanetsCl3 i (PlanetCl3 posi veli' massi)
                                    pokeVCl3 pPlanetsCl3 j (PlanetCl3 posj velj' massj)
                                    loop (j+1)
                     loop (i+1)
                     advance' (i+1)
      
      advance'' i = when (i < length planetsCl3) $ do  -- loops through all of the planets and updates the position
                      (PlanetCl3 (toV3 -> posi) (toV3 -> veli) massi) <- peekElemOff pPlanetsCl3 i
                      let !posi' = posi + R dt * veli
                      pokeCCl3 pPlanetsCl3 i (PlanetCl3 posi' veli massi)
                      advance'' (i+1)
  
  advance' 0   -- update all of the planets velocities
  advance'' 0  -- update all of the planets positions
-- end of advanceCl3


advanceAPS :: Ptr PlanetCl3 -> IO ()
advanceAPS pPlanetsCl3 = do
  let advance' i = when (i < length planetsCl3) $ do  -- loops throught all the planets and updates the velocity
                            let loop j = when (j < length planetsCl3) $ do
                                      ii <- peekElemOff pPlanetsCl3 i
                                      jj <- peekElemOff pPlanetsCl3 j
                                      let !mag = R dt / (dSquared * abs dpos)
                                          !dSquared = dpos * dpos
                                          !dpos = posCl3 ii - posCl3 jj
                                      pokeVCl3 pPlanetsCl3 i ii{velCl3 = velCl3 ii - dpos * massCl3 jj * mag}
                                      pokeVCl3 pPlanetsCl3 j jj{velCl3 = velCl3 jj + dpos * massCl3 ii * mag}
                                      loop (j+1)
                            loop (i+1)
                            advance' (i+1)
      
      advance'' i = when (i < length planetsCl3) $ do  -- loops through all of the planets and updates the position
                      p <- peekElemOff pPlanetsCl3 i
                      pokeCCl3 pPlanetsCl3 i p{posCl3 = posCl3 p + R dt * velCl3 p}
                      advance'' (i+1)
  
  advance' 0   -- update all of the planets velocities
  advance'' 0  -- update all of the planets positions
-- end of advanceAPS




data Planet = Planet {x :: !Double,
                      y :: !Double,
                      z :: !Double,
                      vx :: !Double,
                      vy :: !Double,
                      vz :: !Double,
                      mass :: !Double } deriving (Show)

planets :: [Planet]
planets = [sun, jupiter, saturn, uranus, neptune]


-- | A planet can be described by three cliffors.
data PlanetCl3 = PlanetCl3 {posCl3 :: !Cl3,
                            velCl3 :: !Cl3,
                            massCl3 :: !Cl3} deriving (Show)

planetsCl3 :: [PlanetCl3]
planetsCl3 = [PlanetCl3 sunPos sunVel sunMass,
              PlanetCl3 jupiterPos jupiterVel jupiterMass,
              PlanetCl3 saturnPos saturnVel saturnMass,
              PlanetCl3 uranusPos uranusVel uranusMass,
              PlanetCl3 neptunePos neptuneVel neptuneMass]



sun :: Planet
sun = Planet {x = 0, y = 0, z = 0,
              vx = 0, vy = 0, vz = 0,
              mass = solarMass
              }


sunPos :: Cl3
sunPos = V3 0 0 0
sunVel :: Cl3
sunVel = V3 0 0 0
sunMass :: Cl3
sunMass = R solarMass


jupiter :: Planet 
jupiter = Planet 
    {x = 4.84143144246472090e+00, y = -1.16032004402742839e+00, z= -1.03622044471123109e-01,
     vx = 1.66007664274403694e-03*dp, vy = 7.69901118419740425e-03*dp, vz = -6.90460016972063023e-05*dp,
     mass = 9.54791938424326609e-04 * solarMass
     }


jupiterPos :: Cl3
jupiterPos = V3 (4.84143144246472090e+00) (-1.16032004402742839e+00) (-1.03622044471123109e-01)
jupiterVel :: Cl3
jupiterVel = V3 (1.66007664274403694e-03*dp) (7.69901118419740425e-03*dp) (-6.90460016972063023e-05*dp)
jupiterMass :: Cl3
jupiterMass = R (9.54791938424326609e-04 * solarMass)


saturn :: Planet
saturn = Planet
    {x = 8.34336671824457987e+00, y = 4.12479856412430479e+00, z = -4.03523417114321381e-01,
     vx = -2.76742510726862411e-03*dp,  vy = 4.99852801234917238e-03*dp, vz = 2.30417297573763929e-05*dp,
     mass = 2.85885980666130812e-04 * solarMass
     }


saturnPos :: Cl3
saturnPos = V3 (8.34336671824457987e+00) (4.12479856412430479e+00) (-4.03523417114321381e-01)
saturnVel :: Cl3
saturnVel = V3 (-2.76742510726862411e-03*dp) (4.99852801234917238e-03*dp) (2.30417297573763929e-05*dp)
saturnMass :: Cl3
saturnMass = R (2.85885980666130812e-04 * solarMass)


uranus :: Planet
uranus = Planet
    {x = 1.28943695621391310e+01,y = -1.51111514016986312e+01,z = -2.23307578892655734e-01,
     vx = 2.96460137564761618e-03*dp,vy = 2.37847173959480950e-03*dp, vz = -2.96589568540237556e-05*dp,
     mass = 4.36624404335156298e-05 * solarMass
     }


uranusPos :: Cl3
uranusPos = V3 (1.28943695621391310e+01) (-1.51111514016986312e+01) (-2.23307578892655734e-01)
uranusVel :: Cl3
uranusVel = V3 (2.96460137564761618e-03*dp) (2.37847173959480950e-03*dp) (-2.96589568540237556e-05*dp)
uranusMass :: Cl3
uranusMass = R (4.36624404335156298e-05 * solarMass)


neptune :: Planet
neptune = Planet
    {x = 1.53796971148509165e+01,y = -2.59193146099879641e+01,z = 1.79258772950371181e-01,
     vx = 2.68067772490389322e-03*dp,vy = 1.62824170038242295e-03*dp, vz = -9.51592254519715870e-05*dp,
     mass = 5.15138902046611451e-05 * solarMass
     }


neptunePos :: Cl3
neptunePos = V3 (1.53796971148509165e+01) (-2.59193146099879641e+01) (1.79258772950371181e-01)
neptuneVel :: Cl3
neptuneVel = V3 (2.68067772490389322e-03*dp) (1.62824170038242295e-03*dp) (-9.51592254519715870e-05*dp)
neptuneMass :: Cl3
neptuneMass = R (5.15138902046611451e-05 * solarMass)

daysPerYear :: Double
daysPerYear = 365.24

solarMass :: Double
solarMass = 4 * pi^2
dp :: Double
dp = daysPerYear
dt :: Double
dt = 0.01



-- | 'Storable' instance of Planet
instance Storable Planet where
  sizeOf _ = 8 * dblSz
  alignment _ = dblSz
  peekElemOff p i = peek (plusPtr p (i * sizeOf (undefined :: Planet)))
  pokeElemOff p i e = poke (plusPtr p (i * sizeOf e)) e
  peek p = do
    x' <- peek (offset 0)
    y' <- peek (offset 1)
    z' <- peek (offset 2)
    vx' <- peek (offset 3)
    vy' <- peek (offset 4)
    vz' <- peek (offset 5)
    mass' <- peek (offset 6)
    return Planet {x=x',y=y',z=z',vx=vx',vy=vy',vz=vz',mass=mass'}
      where
        offset i = plusPtr (castPtr p :: Ptr Double) (i*8)
  poke p e = do
    poke (offset 0) $ x e
    poke (offset 1) $ y e
    poke (offset 2) $ z e
    poke (offset 3) $ vx e
    poke (offset 4) $ vy e
    poke (offset 5) $ vz e
    poke (offset 6) $ mass e
      where
        offset i = plusPtr (castPtr p :: Ptr Double) (i*8)

dblSz :: Int
dblSz = sizeOf (undefined :: Double)


-- | 'Storable' instance of PlanetCl3
instance Storable PlanetCl3 where
  sizeOf _ = 4 * cl3Sz
  alignment _ = cl3Sz
  peekElemOff p i = peek ( p `plusPtr` (i * sizeOf (undefined :: PlanetCl3)))
  pokeElemOff p i e = poke (p `plusPtr` (i * sizeOf e)) e
  peek p = do
    pos' <- peek (offset 0)
    vel' <- peek (offset 1)
    mass' <- peek (offset 2)
    return $ PlanetCl3 pos' vel' mass'
      where
        offset i = (castPtr p :: Ptr Cl3) `plusPtr` (i * cl3Sz)
  poke p e = do
    poke (offset 0) $! posCl3 e
    poke (offset 1) $! velCl3 e
    poke (offset 2) $! massCl3 e
      where
        offset i = (castPtr p :: Ptr Cl3) `plusPtr` (i * cl3Sz)

cl3Sz :: Int
cl3Sz = sizeOf (undefined :: Cl3)


-- | 'pokeC' update the position
pokeC :: Ptr Planet -> Int -> Planet -> IO ()
pokeC p i e = do
  poke (offset 0) $ x e
  poke (offset 1) $ y e
  poke (offset 2) $ z e
    where
      offset o = (castPtr p :: Ptr Double) `plusPtr` (o*8+i*64)


pokeCCl3 :: Ptr PlanetCl3 -> Int -> PlanetCl3 -> IO ()
pokeCCl3 p !i !e =
  poke ((castPtr p :: Ptr Cl3) `plusPtr` (i * sizeOf (undefined :: PlanetCl3))) $! posCl3 e


-- | 'pokeV' update the velocity
pokeV :: Ptr Planet -> Int -> Planet -> IO ()
pokeV p i e = do
  poke (offset 3) $ vx e
  poke (offset 4) $ vy e
  poke (offset 5) $ vz e
    where
      offset o = (castPtr p::Ptr Double) `plusPtr` (o*8+i*64)


pokeVCl3 :: Ptr PlanetCl3 -> Int -> PlanetCl3 -> IO ()
pokeVCl3 p !i !e =
  poke ((castPtr p :: Ptr Cl3) `plusPtr` (cl3Sz + i * sizeOf (undefined :: PlanetCl3))) $! velCl3 e


-- | 'fromList' initialize the planets in the stack
fromList :: [Planet] -> IO (Ptr Planet)
fromList l = do
  let len = length l
  pa <- mallocBytes (len * sizeOf (undefined :: Planet))
  let loop [] _ = return ()
      loop (q:qs) i = do
        poke (pa `plusPtr` (i * sizeOf (undefined :: Planet))) q
        loop qs (i+1)
  loop l 0
  return pa


fromListCl3 :: [PlanetCl3] -> IO (Ptr PlanetCl3)
fromListCl3 l = do
  let len = length l
  pa <- mallocBytes (len * sizeOf (undefined :: PlanetCl3))
  let loop [] _ = return ()
      loop (q:qs) i = do
        poke (pa `plusPtr` (i * sizeOf (undefined :: PlanetCl3))) q
        loop qs (i+1)
  loop l 0
  return pa

 




-------------------------------------------------------------------
--
-- The Computer Language
-- Benchmarks Game
-- Revised BSD license
-- 
-- This is a specific instance of the Open Source Initiative (OSI) BSD license template.
-- 
-- Copyright © 2004-2008 Brent Fulgham, 2005-2017 Isaac Gouy
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
-- 
-- > Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
-- 
-- > Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
-- 
-- > Neither the name of "The Computer Language Benchmarks Game" nor the name of "The Computer Language Shootout Benchmarks" nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission. 
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
-- 
-- 
-- 
-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- Contributed by Branimir Maksimovic
--
-------------------------------------------------------------------
