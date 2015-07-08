module Collisionable
(
Collisionable(..)
)
where
import Vector2D
import Circle


class Collisionable a where
	getCollision :: a -> a -> Maybe Vector2D

instance Collisionable Circle where
	getCollision c c'
		| lenPow2 < rDistance ^ 2 = Just (center `sub` center')
		| otherwise = Nothing
			where
				center = getCenter c
				center' = getCenter c'
				rDistance = (getR c) + (getR c')
				lenPow2 = distancePower2 center center'

main = do
	let c = Circle (Vector2D 1 1) 1
	let c' = Circle (Vector2D 2 1) 1
	print (getCollision c c')
