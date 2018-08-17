# Revision history for cl3

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

