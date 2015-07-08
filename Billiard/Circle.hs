module Circle
(
Circle(..)
, CircleAction(..)
, Vector2D(..)
)
where
import Vector2D

type Radius = Double

data Circle = Circle Vector2D Radius deriving (Eq, Show)

class CircleAction a where
	getR, getD :: a -> Radius
	getCenter :: a -> Vector2D
	move :: a -> Vector2D -> a

instance CircleAction Circle where
	getR (Circle c r) = r
	getD (Circle c r) = r * 2
	getCenter (Circle c r) = c
	move (Circle c r) v' = Circle (c `add` v') r
