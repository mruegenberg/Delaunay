module Main where
import Test.HUnit(Counts(..),Test(..),runTestTT,(~?=),(~:))
import Control.Monad (unless)
import Data.Vector.V2
import System.Exit (exitFailure)

-- modules under test
import Graphics.Triangulation.Delaunay (triangulate)

-- TODO: use test-framework
success :: Counts -> Bool
success cs = errors cs == 0 && failures cs == 0

tests ::  IO Counts
tests = runTestTT $ TestList 
  [ fewPointsTests
  , triPointsAreDistinctTests
  ]

fewPointsTests = TestList 
  [ "triangulate no points"  ~: triangulate []                     ~?= []
  , "triangulate one point"  ~: triangulate [v2 1 1]               ~?= []
  , "triangulate two points" ~: triangulate [v2 1 1, v2 2 2]       ~?= []
  , "triangulate many dups"  ~: triangulate (replicate 5 (v2 1 1)) ~?= []
  ]
  where
    v2 = Vector2 

triPointsAreDistinctTests = TestList 
  [ "each tri has distinct pts" ~: filter (not . hasDistinctPoints) (triangulate pts) ~?= []
  ]
  where 
    pts = [v 0 7, v 24 33, v 10 13, v 20 0, v 22 11] where v = Vector2

hasDistinctPoints :: (Vector2, Vector2, Vector2) -> Bool
hasDistinctPoints (p1,p2,p3) = p1/=p2 && p1/=p3 && p2/=p3

main = do
  result <- tests
  unless (success result) exitFailure

