module GUI where
import qualified Circle as K
import qualified Vector2D as K
import Graphics.Gloss

-- Стол: 1.27 * 2.54 метра + 6см на упругий борт
-- Луза: 12 см = 0.12 м
-- Шар: 5,715 см = 0.05715 м

ballD = 0.05715
ballR = ballD / 2

realTableH = 1.27 + 0.03
realTableW = 2.54 + 0.03

tableX = 1000
tableY = tableX / (realTableW / realTableH)
koefMToPx = realTableW / tableX

type Coord = Double
type OffsetPx = Double
type KoefMtoPx = Double

mToPx :: KoefMtoPx -> OffsetPx -> Coord -> Float
mToPx koef offset coord = fromIntegral (round ((coord / koef) + offset)) :: Float

mToPxX = mToPx koefMToPx (-(tableX / 2))
mToPxY = mToPx koefMToPx (-(tableY / 2))
mToPxR = mToPx koefMToPx 0


--TODO: Сделать нормальную расстановку шаров.
--      Номер ряда является максимальным множителем для радиуса.
--      Кличество шаров в ряду равно номеру ряда
--      минус единица. Это справедливо для радов больше первого.
circs =
	[
	K.Circle (K.Vector2D 0.85 0.65) ballR

	, K.Circle (K.Vector2D 0.8 (0.65 - ballR)) ballR
	, K.Circle (K.Vector2D 0.8 (0.65 + ballR)) ballR

	, K.Circle (K.Vector2D 0.75 (0.65 - 2 * ballR)) ballR
	, K.Circle (K.Vector2D 0.75 (0.65)) ballR
	, K.Circle (K.Vector2D 0.75 (0.65 + 2 * ballR)) ballR

	, K.Circle (K.Vector2D 0.7 (0.65 - 3 * ballR)) ballR
	, K.Circle (K.Vector2D 0.7 (0.65 - ballR)) ballR
	, K.Circle (K.Vector2D 0.7 (0.65 + ballR)) ballR
	, K.Circle (K.Vector2D 0.7 (0.65 + 3 * ballR)) ballR

	, K.Circle (K.Vector2D 0.65 (0.65 - 4 * ballR)) ballR
	, K.Circle (K.Vector2D 0.65 (0.65 - 2 * ballR)) ballR
	, K.Circle (K.Vector2D 0.65 (0.65)) ballR
	, K.Circle (K.Vector2D 0.65 (0.65 + 2 *ballR)) ballR
	, K.Circle (K.Vector2D 0.65 (0.65 + 4 * ballR)) ballR
	]

circleToPicture :: K.Circle -> Picture
circleToPicture (K.Circle (K.Vector2D x y) r) = Translate (mToPxX x) (mToPxY y) (Circle (mToPxR r))


main :: IO ()
main
 = 	animate (InWindow "game" (round tableX, round tableY) (5, 5))
        (white)
		frame

frame :: Float -> Picture
frame timeS
 = Pictures
	(map circleToPicture circs)
