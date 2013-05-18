{-# LANGUAGE TemplateHaskell #-} -- for quickCheckAll
module Main where

-- import Test.Framework (Test, defaultMain, testGroup)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Data.List (nub)
import Data.Vector.V2
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.All (quickCheckAll)

-- modules under test
import Graphics.Triangulation.Delaunay (triangulate)
 
hasDistinctPoints :: (Vector2, Vector2, Vector2) -> Bool
hasDistinctPoints (p1,p2,p3) = p1/=p2 && p1/=p2 && p2/=p3

-- this instance makes rounded-value points for 
-- test-data readability.
instance Arbitrary Vector2
  where arbitrary = do 
          let r :: Int -> Double
              r = fromIntegral
          Vector2 <$> fmap r arbitrary <*> fmap r arbitrary

prop_nodups ::  [Vector2] -> Property
prop_nodups pts =
    (length (nub pts) >= 3)  ==>
    -- TODO: reject colinear points
        all hasDistinctPoints (triangulate pts) 
      
 
runTests ::  IO Bool
runTests = $quickCheckAll

main ::  IO ()
main = do
  result <- runTests
  unless result exitFailure
