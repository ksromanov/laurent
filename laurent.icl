/* Полиномы Лорана, скопированные с numeric-prelude */
module laurent
import StdEnv
import StdOverloaded
import StdMaybe

// Полином Лорана - минимальная степень expon
// может быть как больше, так и меньше 0.
// коэффициенты [coeffs] отсчитываются от expon выше.
:: Laurent a = { expon :: Int, coeffs :: !.[a] }

// Конструкторы (константа, коэффициенты полинома, смещённый полином)
const :: a -> Laurent a
const x = fromCoeffs [x]

fromCoeffs :: [a] -> Laurent a
fromCoeffs coeffs = fromShiftCoeffs 0 coeffs

fromShiftCoeffs :: Int [a] -> Laurent a
fromShiftCoeffs expon coeffs = { expon = expon, coeffs = coeffs }

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
    toString { expon, coeffs } = case stringMonomials of
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

instance + (Laurent a) | fromReal a & toReal a & + a where
    (+) { expon = a_expon, coeffs = a } { expon = b_expon, coeffs = b } =
        { expon = min a_expon b_expon, coeffs = opShifted (+) (a_expon - b_expon) a b}

instance - (Laurent a) | fromReal a & toReal a & - a where
    (-) { expon = a_expon, coeffs = a } { expon = b_expon, coeffs = b } =
        { expon = min a_expon b_expon, coeffs = opShifted (-) (a_expon - b_expon) a b}

// Сложение или вычитание со сдвигом,
//  позитивный сдвиг - px начинается раньше
//  негативный сдвиг - py начинается раньше
opShifted :: (a a -> a) Int [a] [a] -> [a] | fromReal a & toReal a
opShifted op del px py
    | del > 0  = opShifted op (del - 1) [(fromReal 0.0):px] py
    | del == 0 = zipWith` op px py
    | del < 0  = opShifted op (del + 1) px [(fromReal 0.0):py]
    where zipWith` _ [] py = map op` py
          zipWith` _ px [] = px
          zipWith` op [x:px] [y:py] = [op x y : zipWith` op px py]
          op` x => op (fromReal 0.0) x

// Стирание лишних нулей с разных сторон
trim :: (Laurent a) -> (Laurent a) | toReal a
trim { expon = expon, coeffs = coeffs } =
            { expon = expon + length zeroes, coeffs = coeffs`` }
    where (zeroes, coeffs`) = span (\a -> toReal a == 0.0) coeffs
          (coeffs``, _)     = span (\a -> toReal a <> 0.0) coeffs`

instance * (Laurent a) | fromReal a & toReal a & * a & + a where
    (*) { expon = a_expon, coeffs = a } { expon = b_expon, coeffs = b } =
        { expon = a_expon + b_expon, coeffs = mul_poly a b }
        where scale v poly = map ((*) v) poly
              mul_poly :: [a] [a] -> [a]
              mul_poly ys xs = abort ""

multiply :: (Laurent a) (Laurent a) -> (Laurent a) | fromReal a & + a & * a
multiply { expon = exp_a, coeffs = coeffs_a } { expon = exp_b, coeffs = coeffs_b } =
    { expon = exp_a + exp_b, coeffs = go coeffs_a [] coeffs_b ++ return coeffs_a coeffs_b}
    where go [a:a_tail] [] b = go a_tail [a] b
          go [a:a_tail] rest_a b = [sum rest_a b : go a_tail [a : rest_a] b]
          go [] _ _ = []

          return _ [] = []
          return a b=:[_:b_tail] = [sum a b : return a b_tail]

          sum :: [a] [a] -> a | fromReal a & + a & * a
          sum a b = foldl (+) (fromReal 0.0) (zipWith (*) a b)

// Возможно стоит использовать библиотечную.
zipWith :: !(a b -> c) ![a] ![b] -> [c]
zipWith _ [] py = []
zipWith _ px [] = []
zipWith op [x:px] [y:py] = [op x y : zipWith op px py]

//                foldr (\y zs -> let [v:vs] = scale y xs in [v : (vs + zs)]) [] b

/*
scale :: a (Laurent a) -> (Laurent a) | * a
scale s { expon = expon, coeffs = coeffs } =
            { expon = expon, coeffs = map ((*) s) coeffs }
*/

Start :: [String]
Start =
        map toString
        [ trim {expon = -2, coeffs = [0.0, 2.0, 1.0, -0.5, 0.0]},
          {expon = -3, coeffs = [1.0, 0.0, 2.0, 1.0, 1.0, 0.0, 6.0, ~2.2]},
          {expon = 0, coeffs = [1.0, 2.0]} + {expon = 1, coeffs = [1.0, 2.0]},
          {expon = 0, coeffs = [1.0, 2.0]} + {expon = 1, coeffs = [1.0, 2.0]},
          multiply a b
        ]
        where a = {expon = 0, coeffs = [1.0, 2.0]}
              b = {expon = 1, coeffs = [1.0, 2.0]}
