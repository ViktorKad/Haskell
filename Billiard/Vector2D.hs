module Vector2D 
(
Vector2D(..)
, Vector2DMath(..)
, Vector2DAction(..)
)
where

-- Алиас для типа данных, реализующего скалярную величину
type Scalar = Double
-- Алиас для типа данных, реализующего угол в радианах
type Angle = Double
-- Common coordinate type
type Coord = Double
-- Алиас для типа данных, реализующего длины, расстояния и т.д.
type Distance = Double

{- 
Тип данных реализующий двумерный вектор из начала 
координат до точки с координатами x y 
-}
data Vector2D = Vector2D Coord Coord  deriving (Eq, Show)

{- 
Интерфейс векторной алгебры и дополнительных 
параметров вектора (длинна, расстояние до другого вектора)
-}
class Vector2DMath a where
	-- Сложение и вычитание векторов
	add, sub :: a -> a -> a
	-- Умножение вектора на число
	mul :: a -> Scalar -> a
	-- Получение длины и квадрата длины вектора
	-- Квадрат длины - более быстрая реализация чем просто длина
	getLength, getLengthPower2 :: a -> Distance
	-- Получить угол (в радианах) между вектором и положительным направлением оси X
	getAngle :: a -> Angle
	-- Расстояние от одного ветора до дрегого
	distancePower2 :: a -> a -> Distance
	-- Получить нормализованный вектор
	getNorm :: a -> a

{- Интерфейс поддержки действий над векторами (поворот, перемещение) -}
class Vector2DAction a where
	-- Получить копию вектора
	copy :: a -> a
	-- Переместить вектор на точку с координатами (x, y)
	moveTo :: a -> (Coord, Coord) -> a
	-- Повернуть вектор на угол
	rotate :: a -> Angle -> a

instance Vector2DMath Vector2D where
	add (Vector2D x y) (Vector2D x' y') = Vector2D (x + x') (y + y')
	sub (Vector2D x y) (Vector2D x' y') = Vector2D (x - x') (y - y')
	mul (Vector2D x y) n = Vector2D (x * n) (y * n)
	getLengthPower2 (Vector2D x y) = x^2 + y^2
	getLength v = sqrt (getLengthPower2 v)
	getAngle (Vector2D x y)
		| angle < 0 = angle + 2 * pi
		| otherwise = angle
		where angle = atan2 x y
	distancePower2 (Vector2D x y) (Vector2D x' y') =
		let
			dX = x - x'
			dY = y - y'
		in dX^2 + dY^2
	getNorm (Vector2D x y) = 
		let 
			len = getLength $ Vector2D x y 
		in Vector2D (x / len) (y / len)

instance Vector2DAction Vector2D where
	copy (Vector2D x y) = Vector2D x y
	moveTo (Vector2D _ _) (x, y) = Vector2D x y
	rotate (Vector2D x y) angle = 
		let
			sinA = sin angle
			cosA = cos angle
			x' = (x * cosA) - (y * sinA)
			y' = (x * sinA) + (y * cosA)
		in Vector2D x' y'

