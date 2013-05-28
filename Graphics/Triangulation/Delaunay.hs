{-#LANGUAGE BangPatterns #-}
module Graphics.Triangulation.Delaunay(triangulate) where

import Data.Vector.V2
import Data.Vector.Class

import Data.BoundingBox.B2(bound_points,BBox2(..))

import Data.Hashable(Hashable(..))
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.HashSet(HashSet)
import qualified Data.HashSet as HSet
import Data.Maybe(fromJust)

import Data.List




-- | Generate the Delaunay triangulation of a set of points
triangulate :: [Vector2] -> [(Vector2,Vector2,Vector2)]
triangulate [] = []
triangulate pts' = 
  case pts of
    (_:_:_:_) -> map (\(Pt a,Pt b,Pt c) -> (a,b,c)) $ triangulationToTris $ removeHelperPoints pts trig
    _tooFew   -> []
  where trig = addPoints (baseTriangulation pts) pts
        pts  = map Pt $ nub pts'
        

----------- Types ----------- 
        
type Triangle = (Pt,Pt,Pt)
        
mkTri :: Pt -> Pt -> Pt -> Triangle
mkTri a b c = (a,b,c)
        
newtype Pt = Pt Vector2 deriving (Eq)

instance Hashable Pt where
  hashWithSalt s (Pt (Vector2 x y)) = hashWithSalt s (x,y)  
  
instance Ord Pt where
  compare (Pt (Vector2 x y)) (Pt (Vector2 x' y')) = case compare x x' of
    EQ -> compare y y'
    c -> c
    
instance Show Pt where
  show (Pt (Vector2 x y)) = case (show x,show y) of (x',y') -> "(" ++ clean x' ++ "," ++ clean y' ++ ")"
    where clean str = case stripPrefix "0." (reverse str) of
            Just str' -> reverse str'
            Nothing -> str
            
            
            
-- a type for unordered pairs
data UPair a = UPair !a !a
mkUPair :: (Ord a) => a -> a -> UPair a
mkUPair a b = if a <= b then UPair a b else UPair b a

instance (Eq a) => Eq (UPair a) where
  (UPair a b) == (UPair c d) = (a == c && b == d)
  
instance (Hashable a, Ord a) => Hashable (UPair a) where
  hashWithSalt s (UPair a b) = hashWithSalt s (a,b)
  
instance (Show a) => Show (UPair a) where
  show (UPair a b) = "{" ++ (show a) ++ ", " ++ (show b) ++ "}"
  
-- `other x y` returns Nothing if x is not part of y and `Just b` where a,b are part of a and x == a
other :: (Eq a) => a -> UPair a -> Maybe a
other x (UPair a b) = if x == a then Just b else if x == b then Just a else Nothing

pElem :: (Eq a) => a -> UPair a -> Bool
pElem x p = case other x p of
  Just _ -> True ; _ -> False
  
  
  
type Triangulation = HashMap Pt (HashSet (UPair Pt))
            
            
----------- Toplevel helpers ----------- 


-- remove points that are in the triangulation only due to the baseTriangulation
removeHelperPoints :: [Pt] -> Triangulation -> Triangulation
removeHelperPoints pts trig = removeHelperPoints' ((HMap.keys trig) \\ pts) trig
  where
    removeHelperPoints' [] trig' = trig'
    removeHelperPoints' (p:ps) trig' = case HMap.lookup p trig' of
      Just nbors -> removeHelperPoints' ps $ 
                   HMap.delete p $
                   HSet.foldl'
                   (\trig'' (UPair nbor1 nbor2) -> HMap.adjust (HSet.filter (not . (p `pElem`))) nbor1 $
                                                  HMap.adjust (HSet.filter (not . (p `pElem`))) nbor2 $
                                                  trig'')
                   trig' nbors
      Nothing -> removeHelperPoints' ps trig' -- shouldn't happen

                           
-- build a triangulation that covers all the points
baseTriangulation :: [Pt] -> Triangulation
baseTriangulation pts = foldl' insertTriangle emptyTriangulation [mkTri p1 p2 p3, mkTri p2 p3 p4]
  where p1 = Pt $ Vector2 xMin yMin
        p2 = Pt $ Vector2 xMin yMax
        p3 = Pt $ Vector2 xMax yMin
        p4 = Pt $ Vector2 xMax yMax
        -- note: p1 <= p2 <= p3 <= p4.
        (BBox2 xMin' yMin' xMax' yMax') = bound_points $ map (\(Pt x) -> x) pts
        [xMin,yMin] = map (\x -> x - 1) [xMin',yMin']
        [xMax,yMax] = map (\x -> x + 1) [xMax',yMax']
        

triangulationToTris :: Triangulation -> [Triangle]
triangulationToTris trig = concatMap (\(p1,nPts) -> map (\(UPair p2 p3) -> (p1,p2,p3)) (HSet.toList nPts)) ptsWithNeighbors
  where
    pts = HMap.keys trig
    ptsWithNeighbors = map (\pt -> (pt, trig HMap.! pt)) pts
    
    
emptyTriangulation :: Triangulation
emptyTriangulation = HMap.empty

insertTriangle :: Triangulation -> Triangle -> Triangulation
insertTriangle !nbors (p1',p2',p3') = newTriangulation
  where newTriangulation = foldl (\nbrs (pt,rst) -> HMap.insertWith HSet.union pt rst nbrs) nbors
                           [(p1,HSet.singleton $ mkUPair p2 p3),
                            (p2,HSet.singleton $ mkUPair p1 p3),
                            (p3,HSet.singleton $ mkUPair p1 p2)]
        [p1,p2,p3] = sort $ [p1', p2', p3']
        
deleteTriangle :: Triangulation -> Triangle -> Triangulation
deleteTriangle !trig (p1,p2,p3) = HMap.adjust (HSet.delete $ mkUPair p2 p3) p1 $ 
                                  HMap.adjust (HSet.delete $ mkUPair p1 p3) p2 $ 
                                  HMap.adjust (HSet.delete $ mkUPair p1 p2) p3 $ 
                                  trig


----------- Helpers ----------- 

dist :: Pt -> Pt -> Double
dist (Pt p1) (Pt p2) = vmag (p1 - p2)

angle :: Pt -> Pt -> Pt -> Scalar
angle !pA !pC !pB = if gamma > pi then pi - gamma else gamma
  where
    gamma = abs $ acos ((a*a + b*b - c*c) / (2 * a * b))
    a = dist pC pB
    b = dist pA pC
    c = dist pA pB
    
-- based on barycentric coordinates
-- from http://www.blackpawn.com/texts/pointinpoly/default.html
-- note: this may have a tendency to numerical problems. If possible try to refactor so that there is less multiplication
containsPoint :: Triangle -> Pt -> Bool
containsPoint (Pt p1, Pt p2, Pt p3) (Pt pt) = u >= 0 && v >= 0 && u + v < 1
  where v0 = p3 - p1
        v1 = p2 - p1
        v2 = pt - p1
        dot00 = vdot v0 v0
        dot01 = vdot v0 v1
        dot02 = vdot v0 v2
        dot11 = vdot v1 v1
        dot12 = vdot v1 v2
        
        denom = (dot00 * dot11 - dot01 * dot01)
        u = (dot11 * dot02 - dot01 * dot12) / denom
        v = (dot00 * dot12 - dot01 * dot02) / denom
        
-- note: in practice, this only yields a little benefit
fastContainsPoint :: Triangle -> Pt -> Bool
fastContainsPoint tri@(Pt (Vector2 x1 y1), Pt (Vector2 x2 y2), Pt (Vector2 x3 y3)) pt@(Pt (Vector2 x y)) = 
  case (min (min x1 x2) x3,max (max x1 x2) x3,min (min y1 y2) y3,max (max y1 y2) y3) of
    (xMin,xMax,yMin,yMax) -> xMin <= x && x <= xMax && yMin <= y && y <= yMax && tri `containsPoint` pt
    
    
----------- Triangulation -----------

-- pulls the neighboring triangles out of a triangulation.
-- each of the resuling triangles is of the form (a,b,c) where a and c are in common with the
-- original triangle
neighbors :: Triangulation -> Triangle -> [(Pt,Triangle)]
neighbors trig (p1',p2',p3') = findNeighbors p1' (p2',p3') ++ findNeighbors p2' (p1',p3') ++ findNeighbors p3' (p1',p2')
  where findNeighbors p1 (p2,p3) = HSet.toList $ HSet.map fromJust $ HSet.delete Nothing $ 
                                   HSet.map (\pr -> case (other p2 pr, other p3 pr) of
                                                (Just p3'',Nothing) | p3 /= p3'' -> Just (p3,mkTri p1 p3'' p2)
                                                (Nothing,Just p2'') | p2 /= p2'' -> Just (p2,mkTri p1 p2'' p3)
                                                (_      ,       _)            -> Nothing)
                                   (trig HMap.! p1)
                        
addPoints :: Triangulation -> [Pt] -> Triangulation
addPoints trig [] = trig
addPoints !trig (pt:pts) = addPoints (addPoint trig pt) pts
    
addPoint :: Triangulation -> Pt -> Triangulation
addPoint trig pt = makeDelaunay 
                   trig'
                   splittedTris
  where
    potentialTris = triangulationToTris trig
    tris = filter (`fastContainsPoint` pt) potentialTris
    tri = head tris
    (trig',(t1,t2,t3)) = splitTriangle trig tri pt
    splittedTris = [t1,t2,t3]
    
splitTriangle :: Triangulation -> Triangle -> Pt -> (Triangulation,(Triangle,Triangle,Triangle))
splitTriangle trig (p1, p2, p3) pt = (trig',(t1,t2,t3))
  where
    -- note: p1 <= p2 <= p3
    trig' = foldl' insertTriangle (deleteTriangle trig (p1,p2,p3)) [t1,t2,t3]
    t1 = mkTri p1 p2 pt
    t2 = mkTri p1 p3 pt
    t3 = mkTri p2 p3 pt
    
    
    
----------- Delaunay -----------

makeDelaunay :: Triangulation -> [Triangle] -> Triangulation
makeDelaunay trig [] = trig
makeDelaunay triangulation (t:ts) = 
  case makeDelaunay' triangulation (neighbors triangulation t) of
    Just (trig',insTr,delTr) -> makeDelaunay trig' (foldr (:) (foldl' (flip delete) ts delTr) insTr)
    Nothing -> makeDelaunay triangulation ts
    where
      -- takes a triangle and its neighbors and checks if any edge of the triangle should be flipped
      -- if so, returns the triangulation with the triangle of interest flipped
      makeDelaunay' :: Triangulation -> [(Pt,Triangle)] -> Maybe (Triangulation,[Triangle],[Triangle])
      makeDelaunay' trig nbors = case filter (\(p3,(p1',p3',p2')) -> shouldFlip (p1', p2') p3' p3) nbors of
        [] -> Nothing
        ((p3,(p1',p3',p2')):_) -> Just (flipEdge trig (p1', p2') p3' p3)
        
-- takes two triangles and checks whether they should be flipped.
-- the first two points are the common points, the latter two those not in the other triangle
shouldFlip :: (Pt, Pt) -> Pt -> Pt -> Bool
shouldFlip (p1, p2) p3 p3' = angle p1 p3 p2 + angle p1 p3' p2 > pi &&
                             angle p3 p1 p3' + angle p3 p2 p3' <= pi

flipEdge :: Triangulation -> (Pt,Pt) -> Pt -> Pt -> (Triangulation,[Triangle],[Triangle])
flipEdge trig (a,b) c c' = (foldl' insertTriangle (foldl' deleteTriangle trig delTr) insTr, insTr, delTr)
  where insTr = [mkTri c a c',mkTri c b c']
        delTr = [mkTri a b c, mkTri a b c']