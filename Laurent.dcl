definition module Laurent

from StdOverloaded import class fromInt, class +, class *, class /,
                          class -, class ==, class toString, class toReal

:: Laurent a = { expon :: Int, coeffs :: !.[a] }

// Конструкторы (константа, коэффициенты полинома, смещённый полином)
fromConst :: a -> Laurent a
fromCoeffs :: [a] -> Laurent a
fromShiftCoeffs :: Int [a] -> Laurent a

// Вычисление в точке
evaluateAtPoint :: (Laurent a) a -> a | fromInt a & +a & *a & /a

// Селекторы и свойства
bounds :: (Laurent a) -> (Int, Int)
degree :: (Laurent a) -> Int
shift :: Int (Laurent a) -> (Laurent a)

instance == (Laurent a) | fromInt a & == a
instance + (Laurent a) | fromInt a & + a & == a
instance - (Laurent a) | fromInt a & - a & == a
instance * (Laurent a) | fromInt a & * a & + a
instance / (Laurent a) | fromInt a & / a & - a & == a

// Деление с остатком
divmod :: (Laurent a) (Laurent a) -> ((Laurent a), (Laurent a)) | fromInt a & / a & - a & == a

// Стирание лишних нулей с разных сторон
trim :: (Laurent a) -> (Laurent a) | fromInt a & == a

instance toString (Laurent a) | toString a & toReal a
