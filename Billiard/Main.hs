module Main where
import qualified Vector2D as K
import qualified Circle as K
import Collisionable
import Ball

main :: IO ()

main = do
	let b = Ball (K.Circle (K.Vector2D 1 1) 1)
	let b' = Ball (K.Circle (K.Vector2D 2 1) 1)
	print (getCollision (primitive b) (primitive b'))
