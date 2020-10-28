/* Полиномы Лорана творчески скопированные с numeric-prelude */
module laurent
import StdEnv
import StdOverloaded
import StdMaybe

// Полином Лорана - минимальная степень expon
// может быть как больше, так и меньше 0.
// Коэффициенты [coeffs] отсчитываются от expon выше.
// Степени в [coeffs] расположены справа налево:
//      [1, 2, 3] == 3z^2 + 2z + 1
:: Laurent a = { expon :: Int, coeffs :: !.[a] }

// Конструкторы (константа, коэффициенты полинома, смещённый полином)
const :: a -> Laurent a
const x = fromCoeffs [x]

fromCoeffs :: [a] -> Laurent a
fromCoeffs coeffs = fromShiftCoeffs 0 coeffs

fromShiftCoeffs :: Int [a] -> Laurent a
fromShiftCoeffs expon coeffs = { expon = expon, coeffs = coeffs }

// Вспомогательные значения a
zero :: a | fromInt a
zero = fromInt 0

one :: a | fromInt a
one = fromInt 0

// Вычисление в точке
evaluate :: (Laurent a) a -> a | fromInt a & +a & *a & /a
evaluate { expon, coeffs } x = lowest_power * evaluate_poly x coeffs
    where lowest_power
            | expon > 0 = pow x expon
            | otherwise = pow inv_x (~expon)

          inv_x = one / x

          pow x 0 = one
          pow x n = x * pow x (n - 1)

          evaluate_poly x coeffs = fst
            (foldl (\(sum, x_n) c -> (sum + x_n * c, x_n * x))
                                     (zero, one) coeffs)

// Селекторы и свойства
// Минимальная и максимальная степени
bounds :: (Laurent a) -> (Int, Int)
bounds { expon, coeffs } = (expon, expon + length coeffs - 1)

// Степень полинома Лорана - разница между его мин и макс степенями
degree :: (Laurent a) -> Int
degree { expon = _, coeffs } = length coeffs

shift :: Int (Laurent a) -> (Laurent a)
shift t { expon, coeffs } = { expon = expon + 1, coeffs }

// Красивый вывод на печать, квадратичный по длине (см foldl)
instance toString (Laurent a) | toString a & toReal a where
    toString { expon, coeffs } = case reverse stringMonomials of
            []     -> "0"
            [(a,_):ax] -> foldl (\r (mono, coeff)
                                | toReal coeff > 0.0 -> r +++ "+" +++ mono
                                | otherwise -> r +++ mono) a ax
        where expanded = filter (\(_, c) -> toReal c <> 0.0) (zip ([expon..], coeffs))
              stringMonomials = map (\l=:(_, coeff) -> (toStringMono l, coeff)) expanded
              op exp
                | exp < 0   = '/'
                | otherwise = '*'
              toStringPow exp
                | axp >  1 = "z^" +++ toString axp
                | axp == 1 = "z"
                | otherwise = "" // exp == 0
                where axp = if (exp > 0)
                                exp
                                (~exp)
              toStringMono (exp, coeff)
                | (toReal coeff) <> 1.0 && exp <> 0 = toString coeff +++ {op exp} +++ toStringPow exp
                | (toReal coeff) <> 1.0 = toString coeff +++ toStringPow exp

                // Situation when coeff == 1
                | exp > 0  = toStringPow exp
                | exp < 0  = "1.0/" +++ toStringPow exp
                | exp == 0 = "1"

instance + (Laurent a) | fromInt a & + a & == a where
    (+) { expon = a_expon, coeffs = a } { expon = b_expon, coeffs = b } =
        trim { expon = min a_expon b_expon, coeffs = opShifted (+) (a_expon - b_expon) a b}

instance - (Laurent a) | fromInt a &  - a & == a where
    (-) { expon = a_expon, coeffs = a } { expon = b_expon, coeffs = b } =
        trim { expon = min a_expon b_expon, coeffs = opShifted (-) (a_expon - b_expon) a b}

// Сложение или вычитание со сдвигом,
//  позитивный сдвиг - px начинается раньше
//  негативный сдвиг - py начинается раньше
opShifted :: (a a -> a) Int [a] [a] -> [a] | fromInt a
opShifted op del px py
    | del > 0  = opShifted op (del - 1) [zero:px] py
    | del == 0 = zipWith` op px py
    | del < 0  = opShifted op (del + 1) px [zero:py]
    where zipWith` _ [] py = map op` py
          zipWith` _ px [] = px
          zipWith` op [x:px] [y:py] = [op x y : zipWith` op px py]
          op` x => op zero x

// Стирание лишних нулей с разных сторон
trim :: (Laurent a) -> (Laurent a) | fromInt a & == a
trim { expon = expon, coeffs = coeffs } =
            { expon = expon + length zeroes, coeffs = coeffs`` }
    where (zeroes, coeffs`) = span ((==) zero) coeffs
          (coeffs``, _)     = span (\a -> not (a == zero)) coeffs`

instance * (Laurent a) | fromInt a & * a & + a where
    (*) { expon = exp_a, coeffs = coeffs_a } { expon = exp_b, coeffs = coeffs_b } =
                                            { expon = exp_a + exp_b, coeffs = go coeffs_a [] coeffs_b }

    where go [a:a_tail] rest_a b = [sum [a:rest_a] b : go a_tail [a:rest_a] b]
          go [] rest_a b = return rest_a (tl b)

          return _ [] = []
          return a b=:[_:b_tail] = [sum a b : return a b_tail]

          sum :: [a] [a] -> a | fromInt a & + a & * a
          sum a b = foldl (+) zero (zipWith (*) a b)

// Деление при котором остаток выбрасывается.
instance / (Laurent a) | fromInt a & / a & - a where
    (/) { expon = exp_a, coeffs = coeffs_a } { expon = exp_b, coeffs = coeffs_b } =
                                            abort "not implemented yet"


div :: (Laurent a) -> (Laurent a) | fromInt a
div _ = abort "not implemented"

// Возможно стоит использовать библиотечную.
zipWith :: !(a b -> c) ![a] ![b] -> [c]
zipWith _ [] py = []
zipWith _ px [] = []
zipWith op [x:px] [y:py] = [op x y : zipWith op px py]

Start :: [String]
Start =
        map toString
        [ trim {expon = -2, coeffs = [0.0, 2.0, 1.0, -0.5, 0.0]},
          {expon = -3, coeffs = [1.0, 0.0, 2.0, 1.0, 1.0, 0.0, 6.0, ~2.2]},
          {expon = 0, coeffs = [1.0, 2.0]} + {expon = 1, coeffs = [1.0, 2.0]},
          {expon = 0, coeffs = [1.0, 2.0]} + {expon = 1, coeffs = [1.0, 2.0]},
          a * b,
          const (evaluate a 2.0) + const (evaluate b 2.0),
          const (evaluate (a + b) 2.0),
          (const 1.0) * (const 1.0),
          (const 3.0) * (const 2.0),
          const (evaluate a 2.0) * const (evaluate b 2.0),
          const (evaluate (a * b) 2.0)
        ] ++ ["\n\n\n"] ++
        map toString
        [
          (const 1) * (const 1),
          (const 3) * (const 2),
          (fromCoeffs [2, 3, 4, 5, 6]) * (const 2),
          (const 2) * (fromCoeffs [2, 3, 4, 5, 6]),
          (fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [0, 1])
        ] ++ ["\n\n\n"] ++
        map toString
        [
          (fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [1, 0, 0]),
          (fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [0, 1, 0]),
          (fromCoeffs [0, 1, 0]) * (fromCoeffs [2, 3, 4, 5, 6]),
          (fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [0, 0, 1]),
          const (evaluate ((fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [2, 3, 4, 5, 6])) 8),
          const ((evaluate (fromCoeffs [2, 3, 4, 5, 6]) 8) * (evaluate (fromCoeffs [2, 3, 4, 5, 6]) 8))
        ]
        where a = {expon = 0, coeffs = [1.0, 2.0]}
              b = {expon = 1, coeffs = [1.0, 2.0]}
