/* Полиномы Лорана творчески скопированные с numeric-prelude */
implementation module Laurent
import StdEnv
import StdOverloaded
import StdMaybe
import Data.List

// Полином Лорана - минимальная степень expon
// может быть как больше, так и меньше 0.
// Коэффициенты [coeffs] отсчитываются от expon выше.
// Степени в [coeffs] расположены справа налево:
//      [1, 2, 3] == 3z^2 + 2z + 1
:: Laurent a = { expon :: Int, coeffs :: !.[a] }

// Конструкторы (константа, коэффициенты полинома, смещённый полином)
fromConst :: a -> Laurent a
fromConst x = fromCoeffs [x]

fromCoeffs :: [a] -> Laurent a
fromCoeffs coeffs = fromShiftCoeffs 0 coeffs

fromShiftCoeffs :: Int [a] -> Laurent a
fromShiftCoeffs expon coeffs = { expon = expon, coeffs = coeffs }

// Вспомогательные значения a
zero :: a | fromInt a
zero = fromInt 0

one :: a | fromInt a
one = fromInt 1

// Вычисление в точке
evaluateAtPoint :: (Laurent a) a -> a | fromInt a & +a & *a & /a
evaluateAtPoint { expon, coeffs } x = pow expon * evaluate_poly x coeffs
    where pow n
            | n == 0 = one
            | n > 0  = x * pow (n - 1)
            | otherwise = inv_x * pow (n + 1)

          inv_x = one / x

          evaluate_poly x coeffs = sum
            where (sum, last) = (foldl (\(sum, x_n) c -> (sum + x_n * c, x_n * x))
                                     (zero, one) coeffs)

// Стирание лишних нулей с разных сторон
trim :: (Laurent a) -> (Laurent a) | fromInt a & == a
trim { expon = expon, coeffs = coeffs } =
    case coeffs`` of
        [] -> { expon = 0, coeffs = [] }
        _  -> { expon = expon + length zeroes, coeffs = coeffs`` }
    where (zeroes, coeffs`) = span ((==) zero) coeffs
          (coeffs``, _)     = span (\a -> not (a == zero)) coeffs`

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

instance - (Laurent a) | fromInt a & - a & == a where
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
    where zipWith` _ [] py = map (op zero) py
          zipWith` _ px [] = px
          zipWith` op [x:px] [y:py] = [op x y : zipWith` op px py]

instance == (Laurent a) | fromInt a & == a where
    (==) a b = (a`.expon == b`.expon) && (a`.coeffs == b`.coeffs)
               where a` = trim a
                     b` = trim b

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
instance / (Laurent a) | fromInt a & / a & - a & == a where
    (/) a b = fst (divmod a b)

// Деление полинома a на полином b с остатком. Возвращаемое значение - (частное, остаток)
divmod :: (Laurent a) (Laurent a) -> ((Laurent a), (Laurent a)) | fromInt a & / a & - a & == a
divmod { expon = exp_a, coeffs = coeffs_a } { expon = exp_b, coeffs = coeffs_b } =
          (trim { expon = exp_a - exp_b, coeffs = reverse (fst poly_divided) },
                        trim { expon = exp_a, coeffs = reverse (snd poly_divided) })

       where poly_divided = go (length coeffs_a) (reverse coeffs_a) (length coeffs_b) (reverse coeffs_b)
             go :: Int [a] Int [a] -> ([a], [a]) | fromInt a & / a
             go len_a [a:a_rest] len_b [b:b_rest]
               | len_a < len_b = ([zero], [a:a_rest])
               | otherwise = ([quotient:quotient_rest], residue)
                   where quotient = a/b
                         residue` = zipWith (-) a_rest b_rest ++ drop (len_b - 1) [a:a_rest]
                         (quotient_rest, residue) = go (len_a - 1) a_rest len_b [b:b_rest]
