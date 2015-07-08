module Game
(
balls
) where
import Vector2D
import Circle
import Collisionable
import Ball

--TODO: Частичное описание алгоритма заполнения см. в GUI.hs
balls = [x | x <- [newBall 0 0 0], _ <- [1..9]]

main :: IO ()
main = do
	print 1
	print 2
	print (length balls)
