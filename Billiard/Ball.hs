module Ball (
Ball(..)
, newBall
) where
import Circle
import Collisionable

newtype Ball = Ball { primitive :: Circle } deriving (Eq, Show)

type Coord = Double
type Radius = Double

newBall :: Coord -> Coord -> Radius -> Ball
newBall x y r = Ball (Circle (Vector2D x y) r)

main = do
	print (newBall 1 2 3)
	print "---"
