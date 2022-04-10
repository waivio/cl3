# Revision history for cl3

## 3.0.0.0  -- 2022-04-01

* Added LiquidHaskell support
* Added more memory efficient subtypes for R, V3, BV, I, PV, H, C, BPV, ODD, TPV
* Added LiquidHaskell support: liquidhaskell >= 0.8.10
* Sucessfully tested with: liquidhaskell-0.8.10.2 and Stackage lts-18.6
* Added a LiquidHaskell flag to build: stack build --flag cl3:do-liquid
* LiquidHaskell requires CPP Options: -DO_LIQUID -DO_NO_RANDOM -DO_NO_DERIVED -DO_NO_STORABLE
* LiquidHaskell _|_ when: Random, Storable, or Derived instances are enabled
* LiquidHaskell _|_ when: There is any number n.nn*e^-x see [Issue#1762](https://github.com/ucsd-progsys/liquidhaskell/issues/1762) for my 128*eps threshold, eventhough rationals are supposed to be supported
* Added more memory efficient subtypes for R, V3, BV, I, PV, H, C, BPV, ODD, TPV, and APS
* Namely: Cl3_R, Cl3_V3, Cl3_BV, Cl3_I, Cl3_PV, Cl3_H, Cl3_C, Cl3_BPV, Cl3_ODD, Cl3_TPV, and Cl3_APS
* Added smart constructors to covert to/from the more memory efficient subtypes
* Added a "weigh" based benchmark to verify that the more memory efficient subtypes are actually more memory efficent
* Added a WeighStorableCl3 benchmark; command to run: stack bench cl3:bench-cl3-weigh
* Added a NbodyMassiv benchmark; command to run: stack bench cl3:bench-cl3-massiv-nbody
* Modified existing NbodyGameCl3 benchmark; command to run: stack bench cl3:bench-cl3-nbody
* LiquidHaskell found some interesting unsaftey in the definition of 'tan' and 'tanh'
* Hopefully it's solved with a judicious use of 'reduce' for instances above R and I, in the affected 'tan' and 'tanh' calls
* Corrected spelling error sp: simi to semi, in the definition of a random unit vector
* 'abssignum' why not calculate 'abs' once and use it twice
* 'absolute' only constructs an R


## 2.0.0.0  -- 2020-06-20

* Added work around for GHC 8.10 regression of Issue #15304 reproducing code changes from GHC MR 2608 in the source files
* Added 'BangPatterns' language extension
* Added 'MultiWayIf' language extension
* Added 'Control.DeepSeq' dependency for 'NFData' and 'rnf'
* Added class instance for 'NFData'
* Added 'randUnitary' for a random Unitary value in APS
* Added CPP flags to Cl3 be able to turn off derived instances and the random dependancy
* Added CPP flags to JonesCalculus to turn off the random dependancy
* Added new function 'mIx' for the Inverse Hodge Star operator
* Added new function 'timesI' to easily multiply 'i' times something 
* Fixed 'compare' so that there will be a total order when comparing I with other I values
* Refactored 'compare' so that lets were moved to a higher level
* Refactored 'abs' so that (2*) was changed to (x + x) and common computations were let floated
* Refactored 'abs' to reduce duplicate code with a helper function
* Refactored 'signum' to inline more Double precesion math into the returned value
* Refactored 'signum' to reduce duplicate code with a helper function
* Added 'reimMag' helper function for calculating the magnitude of the real and imaginary grades of APS
* Refactored 'recip' to use a helper function, moved some shared calculations to a 'let' binding
* Removed the final 'reduce' from the Fractional instances
* Refactored 'log' to convert the 'sqrt' from inside the log to a '(/2)'
* Refactored imaginary implementation of 'log' to specialize the values at +/- 1 to be purly imaginary
* Refactored imaginary implementation of 'sqrt' to inline more Double precision math into the 'C' constructor
* Refactored imaginary implementation of 'sqrt' to specialize the values at 0 to be purly real
* Refactored complex implementation of 'sqrt' to inline more Double precision math into the 'C' constructor
* Refactored imaginary implementation of 'sin' to specialize the values at 0 to be purly real
* Refactored complex implementation of 'tan' to inline more Double precision math into the 'C' constructor
* Refactored imaginary implementation of 'tan' to specialize the value at 0 to be purly real
* Refactored real implementation of 'asin' to re-derive the implemenation to inline more Double precision math into the various constructors
* Refactored imaginary implementation of 'asin' to specialize the value at 0 to be purly real
* Refactored complex implementation of 'asin' to inline more Double precision math into the 'C' constructor
* Refactored real implementation of 'acos' to re-derive the implemenation to inline more Double precision math into the various constructors
* Refactored imaginary implementation of 'acos' to specialize the value at 0 to be purly real
* Refactored complex implementation of 'acos' to inline more Double precision math into the 'C' constructor
* Refactored complex implementation of 'acos' to specialize the value at 0 to be purly real
* Refactored imaginary implementation of 'atan' to re-derive the implemenation to inline more Double precision math into the various constructors
* Refactored complex implementation of 'atan' to inline more Double precision math into the 'C' constructor
* Refactored complex implementation of 'tanh' to inline more Double precision math into the 'C' constructor
* Refactored imaginary implementation of 'asinh' to re-derive the implemenation to inline more Double precision math into the various constructors
* Refactored complex implementation of 'asinh' to inline more Double precision math into the 'C' constructor
* Refactored real implementation of 'acosh' to re-derive the implemenation to inline more Double precision math into the various constructors
* Refactored imaginary implementation of 'acosh' to re-derive the implemenation to inline more Double precision math into the various constructors
* Refactored complex implementation of 'acosh' to inline more Double precision math into the 'C' constructor
* Refactored real implementation of 'atanh' to re-derive the implemenation to inline more Double precision math into the various constructors
* Refactored imaginary implementation of 'atanh' to inline more Double precision math into the 'I' constructor
* Refactored imaginary implementation of 'atanh' to specialize the value at 0 to be purly real
* Refactored complex implementation of 'atanh' to inline more Double precision math into the 'C' constructor
* Refactored 'lsv' same as 'abs'
* Refactored 'lsv' to guard the sqrt function so that negative values
* Refactored 'lsv' to use a helper function to reduce duplicated code
* Added 'loDisc' helper function to calculate lsv for PV and TPV
* Implemented hlint's suggestion to remove parens around pattern for 'spectraldcmp' helper function 'dcmp'
* Refactored 'dcmp' to order based on the RHS and to commonize the BPV and APS constructors
* Implemented hlint's suggestion to remove parens around pattern for 'eigvals' helper function 'eigv'
* Refactored 'eigv' to order based on the RHS and to commonize the BPV and APS constructors
* Added 'dup' helper function to duplicate a value in a tuple
* Implemented hlint's suggestion to remove parens around pattern for 'project' helper function 'proj'
* Refactored 'project' to use helper functions for single and double vector grade constructors
* Added 'biTriDProj' helper function for generating projectors for double vector grades
* Added 'triDProj' helper function for generating projectors for single vector grades
* Refactored 'boost2colinear' to specialize and inline more Double precision math
* Refactored 'isColinear' to be calculated with Double precision math with a helper function 'colinearHelper'
* Corrected 'isColinear' to properly test for colinear even with non-reduced values
* Added 'colinearHelper' function to calculate if the biparavector portion is colinear
* Refactored 'hasNilpotent' to be calculated with Double precision math with a helper function 'nilpotentHelper'
* Added 'nilpotentHelper' function to calculate if the biparavector portion is nilpotent
* Implemented hlint's suggestion to remove '$' from 'projEigs'
* Refactored 'reduce' to factor out a shared comparison and use a helper function
* Refactored 'reduce' to re-order some of the comparisons to ones that are more common
* Removed the old value of 'mI'
* Performed the multiplication that was in 'tol' and 'tol''
* Refactored 'recip'' to be in a point free style
* Refactored 'sqrt'' to be in a point free style
* Refactored 'tan'' to be in a point free style
* Refactored 'asin'' to be in a point free style
* Refactored 'acos'' to be in a point free style
* Refactored 'atan'' to be in a point free style
* Refactored 'tanh'' to be in a point free style
* Refactored 'asinh'' to be in a point free style
* Refactored 'atanh'' to be in a point free style
* Added random projectors, nilpotnents, and unitary cliffors, to the Random instance of Cl3
* Refactored 'rangePV' to be more uniform and within the required range
* Refactored 'rangeH' to be more uniform and within the required range
* Refactored 'rangeC' to be more uniform and within the required range
* Refactored 'rangeBPV' to be more uniform and within the required range
* Refactored 'rangeODD' to be more uniform and within the required range
* Refactored 'rangeTPV' to be more uniform and within the required range
* Refactored 'rangeAPS' to be more uniform and within the required range
* Refactored 'randUnitV3' to be more uniform and not to be biased to the poles
* Refactored 'randProjector' to inline more Double precision math into the PV constructor
* Refactored 'randNilpotent' to inline more Double precision math into the BPV constructor
* Added 'randUnitary' to generate random unitary Cliffors
* Refactored 'vectorHelper' to use 'randUnitV3'
* Rewrote the tests to use Criterion instead of QuickCheck
* Changed the tests Arbitrary to 'randomRIO'
* Changed the test's random input to be 5,000,000 Cliffors
* Refactored the tests to use 'mIx'
* Refactored the tests '≈≈' to be a mean squared error calculation compared to a threshold
* Refactored the tests 'poles' to use a 'closeTo' function instead of '≈≈' to compare with eigenvalues
* Added to the tests a 'closeTo' function to compare against eigenvalues in the complex plane using a Euclidean distance

## 1.0.0.4  -- 2018-10-18

* Found various improvements while preparing for NPFL specialized Jordan for BPV and APS
* Removed all $! and replaced with $, found that this resolved compile time and space issues updated ghc track #15304
* Removed -fno-worker-wrapper from the cabal file
* Greatly simplified the implementation of boost2colinear also discovered while preparing for NPFL

## 1.0.0.3  -- 2018-08-16

* Factored out the view pattern (reduce -> cliffor) on several functions so it wasn't repeated in every pattern match
* Added -fno-worker-wrapper to work around the compile time and space issues in ghc > 8.0

## 1.0.0.2  -- 2018-06-19

* Tested with GHC 7.8.4 and 8.4.2
* Lowered version bounds to support lts-2.22, base >= 4.7, QuickCheck >= 2.7
* Explicitly imported <$> from Control.Applicative to support the earlier versions of Base & GHC for the tests

## 1.0.0.1  -- 2018-06-10

* Used Stack to test different versions of GHC.
* Removed {-# OPTIONS_GHC -fno-warn-unused-top-binds #-} from Cl3.hs to better support earlier versions of GHC, and it was no longer needed.
* Loosened version bound for QuickCheck to work better with earlier versions of Stackage LTS snapshots.
* Improved spectraldcmp's documentation to clairify that spectraldcmp requires an implementation of the real, imaginary, and complex implememtation of the function.

## 1.0.0.0  -- 2017-10-28

* First version. Released on an unsuspecting world.

