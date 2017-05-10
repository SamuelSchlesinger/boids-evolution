{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    ) where

import Graphics.Gloss.Interface.Pure.Simulate
import Linear.V2
import Linear
import Control.Lens

neighborhoodC = 0.2
separationF d p = 0.4 *^ p
alignmentF p = 0.25 *^ p
cohesionF  p = 0.3 *^ p

data Boid = Boid {
  _pos  :: V2 Float,
  _vel  :: V2 Float,
  _acc  :: V2 Float,
  _mass :: Float,
  _tag  :: Int }

makeLenses ''Boid

someFunc :: IO ()
someFunc = simulate dmode background fps boids (scale 500 500 . display) update where
  dmode      = InWindow "Boids Evolution" (500, 500) (100, 100)
  background = white
  fps        = 30
  boids      = initialBoids :: [Boid]
  display bs = case bs of
    []       -> blank
    (x : xs) -> displayBoid x `mappend` display xs
  displayBoid b = translate (b^.pos^._x) (b^.pos^._y) 
                $ circleSolid (sqrt (b^.mass / pi))
  update view dt batch = affectedBoids where
    movedBoids = [ over acc (const 0) 
                 $ over vel ((0.99 *^) . (+ (dt *^ b^.acc)))
                 $ over pos (+ (dt *^ b^.vel)) b | b <- batch ]
    affectedBoids = [ b `flockWith` [c | c <- batch
                                       , distance (b^.pos) (c^.pos) < neighborhoodC
                                       , c^.tag /= b^.tag ]
                    | b <- movedBoids ]
    flockWith :: Boid -> [Boid] -> Boid
    flockWith b neighborhood = over acc (const (cohesion + alignment + separation)) b where
      positions  = map _pos neighborhood
      velocities = map _vel neighborhood
      centroid = (recip n) *^ sum positions
      velCentroid = (recip n) *^ sum velocities
      n = fromInteger $ toInteger $ length neighborhood
      pointer    = normalize (centroid - b^.pos)
      velPointer = normalize (velCentroid - b^.vel)
      cohesion   = cohesionF   pointer
      alignment  = alignmentF  velPointer
      separation = sum $ do
        c <- neighborhood
        let disp = b^.pos - c^.pos
        let dist = norm disp
        return (separationF dist disp)

initialBoids = [ Boid (V2 x y) (V2 (sin x) (cos x)) 0 0.001 (n * 20 + m)  | 
                 (n, x) <- zip [0 .. 19] [-1, -0.95 .. 1], 
                 (m, y) <- zip [0 .. 19] [-1, -0.95 .. 1]]
