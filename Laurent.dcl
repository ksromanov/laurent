definition module Laurent

from StdOverloaded import class fromInt, class +, class *, class /, class <,
                          class -, class ==, class toString, class toReal

:: Laurent a = { expon :: !Int, coeffs :: !.[a] }

// Конструкторы (нуль, константа, коэффициенты полинома, смещённый полином)
zeroLaurent :: Laurent a
fromConst :: !a -> Laurent a
fromCoeffs :: ![a] -> Laurent a
fromShiftCoeffs :: !Int ![a] -> Laurent a

// Вычисление в точке
evaluateAtPoint :: !(Laurent a) !a -> a | fromInt a & +a & *a & /a

// Стирание лишних нулей с разных сторон
trim :: !(Laurent a) -> (Laurent a) | fromInt a & == a

// Селекторы и свойства
bounds :: !(Laurent a) -> (Int, Int) | == a & fromInt a
degree :: !(Laurent a) -> Int | == a & fromInt a
shift :: !Int !(Laurent a) -> (Laurent a)

instance == (Laurent a) | fromInt a & == a
instance + (Laurent a) | fromInt a & + a & == a
instance - (Laurent a) | fromInt a & - a & == a
instance * (Laurent a) | fromInt a & * a & + a
instance / (Laurent a) | fromInt a & / a & - a & == a & * a

// Деление с остатком со стороны старших степеней
divmod :: !(Laurent a) !(Laurent a) -> ((Laurent a), (Laurent a)) | fromInt a & / a & - a & == a & * a

// "Полный" спектр решений деления с остатком
divmodSpectrum :: !(Laurent a) !(Laurent a) -> [(Laurent a, Laurent a)] | * a & /a & - a & fromInt a & == a

// Инверсия, т.е. преобразование z -> 1/z
inverse :: !(Laurent a) -> (Laurent a)

// Алгоритм Евклида для нахождения наибольшего общего делителя (используется divmod)
greatestCommonDivisor :: !(Laurent a) !(Laurent a) -> (Laurent a) | fromInt a & / a & - a & == a & * a

// Алгоритм Евклида с полным путём деления (всеми делителями) и произвольным оператором divmod
greatestCommonDivisorPath :: !((Laurent a) (Laurent a) -> (Laurent a, Laurent a)) !(Laurent a) !(Laurent a) -> [(Laurent a)] | fromInt a & == a

instance toString (Laurent a) | toString a & < a & == a & fromInt a
