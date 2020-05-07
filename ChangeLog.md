# Revision history for cl3

## 2.0.0.0  -- 2020-05-07

* Added CPP flags to Cl3 be able to turn off derived instances and the random dependancy
* Added CPP flags to JonesCalculus to turn off the random dependancy
* Added new function 'mIx' for the Inverse Hodge Star operator
* Added new function 'timesI' to easily multiply 'i' times something 
* Fixed 'compare' so that there will be a total order when comparing I with other I values
* Refactored 'compare' so that lets were moved to a higher level
* Refactored 'abs' so that (2*) was changed to (x + x) and common computations were let floated
* Refactored 'recip' to use a helper function, moved some shared calculations to a 'let' binding
* Refactored 'log' to convert the 'sqrt' from inside the log to a '(/2)'
* Refactored imaginary implementation of 'sqrt' to inline more Double precision math into the 'C' constructor
* Refactored complex implementation of 'sqrt' to inline more Double precision math into the 'C' constructor
* Refactored complex implementation of 'tan' to inline more Double precision math into the 'C' constructor
* Refactored real implementation of 'asin' to re-derive the implemenation to inline more Double precision math into the various constructors
* Refactored complex implementation of 'asin' to inline more Double precision math into the 'C' constructor
* Refactored real implementation of 'acos' to re-derive the implemenation to inline more Double precision math into the various constructors
* Refactored complex implementation of 'acos' to inline more Double precision math into the 'C' constructor
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
* Refactored complex implementation of 'atanh' to inline more Double precision math into the 'C' constructor
* Refactored 'lsv' same as 'abs'
* Implemented hlint's suggestion to remove parens around pattern for 'spectraldcmp' helper function 'dcmp'
* Refactored 'dcmp' to order based on the RHS and to commonize the BPV and APS constructors
* Implemented hlint's suggestion to remove parens around pattern for 'eigvals' helper function 'eigv'
* Refactored 'eigv' to order based on the RHS and to commonize the BPV and APS constructors
* Implemented hlint's suggestion to remove parens around pattern for 'project' helper function 'proj'
* Refactored 'proj' to order based on the RHS and to commonize the BPV and APS constructors
* Refactored 'proj' to use 'mIx' and fewer parens
* Refactored 'proj' to distribute the (0.5*) on the real and vector parts
* Refactored 'boost2colinear' to use 'mIx'
* Refactored 'isColinear' to be more directly comparisons on Doubles with a helper function 'hasit'
* Refactored 'isColinear' to use a helper function suggested by hlint to calculate the magnitude of the vector and bivector components
* Refactored 'isColinear' to remove the 'signum' function because it was too big and not needed
* Refactored 'isColinear' to commonize the BPV and APS constructors
* Refactored 'hasNilpotent' to be more directly comparisons on Doubles with a helper function 'hasit'
* Refactored 'hasNilpotent' to use a helper function suggested by hlint to calculate the magnitude of the vector and bivector components
* Refactored 'hasNilpotent' to remove the 'signum' function because it was too big and changed to square the bpv to determine if the magnitude was close to zero
* Refactored 'hasNilpotent' to commonize the BPV and APS constructors
* Added 'vMagHelper' as suggested by hlint to commonize some calculation of the magnitude of the vector and bivector parts of a cliffor
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
* Refactored 'randNilpotent' to use 'mIx'
* Changed the tests to run 50,000 times
* Refactored the tests to use 'mIx'

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

