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
:: Laurent a = { expon :: !Int, coeffs :: !.[a] }

// Конструкторы (нуль, константа, коэффициенты полинома, смещённый полином)
zeroLaurent :: Laurent a
zeroLaurent = { expon = 0, coeffs = [] }

fromConst :: !a -> Laurent a
fromConst x = fromCoeffs [x]

fromCoeffs :: ![a] -> Laurent a
fromCoeffs coeffs = fromShiftCoeffs 0 coeffs

fromShiftCoeffs :: !Int ![a] -> Laurent a
fromShiftCoeffs expon coeffs = { expon = expon, coeffs = coeffs }

// Вспомогательные значения a
zero :: a | fromInt a
zero = fromInt 0

one :: a | fromInt a
one = fromInt 1

// Вычисление в точке
evaluateAtPoint :: !(Laurent a) !a -> a | fromInt a & +a & *a & /a
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
trim :: !(Laurent a) -> (Laurent a) | fromInt a & == a
trim { expon = expon, coeffs = coeffs } =
    case coeffs_rev of
        [] -> { expon = expon, coeffs = [] }
        _  -> { expon = expon + length zeroes, coeffs = reverse coeffs_rev }
    where (zeroes, coeffs`) = span ((==) zero) coeffs
          coeffs_rev        = dropWhile ((==) zero) (reverse coeffs`)

// Селекторы и свойства
// Минимальная и максимальная степени
bounds :: !(Laurent a) -> (Int, Int) | == a & fromInt a
bounds a =: { expon = e } = case coeffs of
      [] -> (e, e)
      _  -> (expon, expon + length coeffs - 1)
    where { expon, coeffs } = trim a

// Степень полинома Лорана - разница между его мин и макс степенями
degree :: !(Laurent a) -> Int | == a & fromInt a
degree a = case coeffs of
      [] -> 0
      _  -> length coeffs - 1
    where { coeffs } = trim a

shift :: !Int !(Laurent a) -> (Laurent a)
shift t { expon, coeffs } = { expon = expon + t, coeffs }

// Красивый вывод на печать, квадратичный по длине (см foldl)
instance toString (Laurent a) | toString a & < a & == a & fromInt a where
    toString { expon, coeffs } = case reverse stringMonomials of
            []     -> "0"
            [(a,_):ax] -> foldl (\r (mono, coeff)
                                | zero < coeff -> r +++ "+" +++ mono
                                | otherwise -> r +++ mono) a ax
        where expanded = filter (\(_, c) -> c <> zero) (zip ([expon..], coeffs))
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
                | coeff <> one && exp <> 0 = toString coeff +++ {op exp} +++ toStringPow exp
                | coeff <> one = toString coeff +++ toStringPow exp

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
opShifted :: !(a a -> a) !Int ![a] ![a] -> [a] | fromInt a
opShifted op del px py
    | del > 0  = opShifted op (del - 1) [zero:px] py
    | del == 0 = zipWith` op px py
    | del < 0  = opShifted op (del + 1) px [zero:py]
    where zipWith` _ [] py = map (op zero) py
          zipWith` _ px [] = px
          zipWith` op [x:px] [y:py] = [op x y : zipWith` op px py]

instance == (Laurent a) | fromInt a & == a where
    (==) a b
        | a`.coeffs == [] = (a`.coeffs == b`.coeffs)
        | otherwise = (a`.expon == b`.expon) && (a`.coeffs == b`.coeffs)
               where a` = trim a
                     b` = trim b

instance * (Laurent a) | fromInt a & * a & + a where
    (*) { expon = _ , coeffs = [] } _ = { expon = 0 , coeffs = [] }
    (*) _ { expon = _ , coeffs = [] } = { expon = 0 , coeffs = [] }
    (*) { expon = exp_a, coeffs = coeffs_a } { expon = exp_b, coeffs = coeffs_b } =
                                            { expon = exp_a + exp_b, coeffs = go coeffs_a [] coeffs_b }

    where go [a:a_tail] rest_a b = [sum [a:rest_a] b : go a_tail [a:rest_a] b]
          go [] rest_a b = return rest_a (tl b)

          return _ [] = []
          return a b=:[_:b_tail] = [sum a b : return a b_tail]

          sum :: [a] [a] -> a | fromInt a & + a & * a
          sum a b = foldl (+) zero (zipWith (*) a b)

// Деление при котором остаток выбрасывается.
instance / (Laurent a) | fromInt a & / a & - a & == a & * a where
    (/) a b = fst (divmod a b)

// Деление полинома a на полином b с остатком. Возвращаемое значение - (частное, остаток)
// Деление происходит со старшей степени до младшей, как обычный полином.
divmod :: !(Laurent a) !(Laurent a) -> ((Laurent a), (Laurent a)) | fromInt a & / a & - a & == a & * a
divmod a` b` =
          (trim { expon = exp_a - exp_b, coeffs = reverse (fst poly_divided) },
                        trim { expon = exp_a, coeffs = reverse (snd poly_divided) })

       where { expon = exp_a, coeffs = coeffs_a } = trim a`
             { expon = exp_b, coeffs = coeffs_b } = trim b`
             poly_divided = go (length coeffs_a) (reverse coeffs_a) (length coeffs_b) (reverse coeffs_b)
             go :: Int [a] Int [a] -> ([a], [a]) | fromInt a & / a & == a & -a & *a
             go _ [] _ _ = ([], [])
             go len_a [a:a_rest] len_b bx=:[b:b_rest]
               | len_a < len_b = ([], [a:a_rest])
               | otherwise = ([quotient:quotient_rest], residue)
                   where quotient = a/b
                         residue` = zipWith (\a b -> a - b*quotient) a_rest b_rest ++ drop (len_b) [a:a_rest]
                         (quotient_rest, residue) = go (len_a - 1) residue` len_b bx

inverse :: !(Laurent a) -> (Laurent a)
inverse a =
    { expon = 1 - a.expon - length a.coeffs, coeffs = reverse a.coeffs }

// Алгоритм Евклида для нахождения наибольшего общего делителя
greatestCommonDivisor :: !(Laurent a) !(Laurent a) -> (Laurent a) | fromInt a & / a & - a & == a & * a
greatestCommonDivisor a` b`
    | degree a > degree b = iter a b
    | otherwise = iter b a

    where (a, b) = (trim a`, trim b`)
          iter a b
            | zeroLaurent == b = a
            | otherwise = iter b rem
                where (quot, rem) = divmod a b

// Деление со всех сторон с построением полного спектра. Возможны дубликаты.
// Сначала генерируем полный спектр по stepLowEnd, а потом
// каждый проходим до конца используя stepHighEnd.
//
// Результаты отсортированы по количеству делений с "головы" с убыванием. Т.е.
// [divmodHighEnd,....., divmodLowEnd], divmodHighEnd = divmod выше.
//
// Кодировка - (q, r): q - частное, r - остаток.
divmodSpectrum :: !(Laurent a) !(Laurent a) -> [(Laurent a, Laurent a)] | * a & /a & - a & fromInt a & == a
divmodSpectrum a` b`
    | b == zero = abort "Attempt to divide by zero polynomial."
    | degree a < degree b = [(zero, a)]
    | otherwise = map (\(q, r) -> ({ q & expon = a.expon - b.expon }, r)) (iterLowEnd a)

    where (a, b) = (trim a`, trim b`)

          zero :: Laurent a
          zero = fromCoeffs []

//        iterLowEnd :: !(Laurent a) -> [(Laurent a, Laurent a)]
          iterLowEnd a
             | length a.coeffs < length_b = [(zero, a)]
             | otherwise = [divmodHighEnd a : pair_map (append q) (iterLowEnd a`)]

                 where a` = { expon = a.expon + 1, coeffs = subtractList q (tl a.coeffs) (tl b.coeffs)}
                       q = (hd a.coeffs/hd b.coeffs)

                       pair_map f lst = map (\(x, y) -> (f x, y)) lst

                       append :: !a !(Laurent a) -> Laurent a
                       append q x = { x & coeffs = [q : x.coeffs] }

                       length_b = length b.coeffs

          // Фактически это просто левое деление на b (со старшей степени).
          // NOTE: полиномы должны быть "trimmed".
//        divmodHighEnd :: !(Laurent a) -> (Laurent a, Laurent a)
          divmodHighEnd a = (fromShiftCoeffs (a.expon - b.expon) q, // NOTE: q переворачивать не надо!!!!
                             fromShiftCoeffs a.expon (reverse r))

              where (q, r) = go [] (reverse a.coeffs) reverse_b_coeffs
                    reverse_b_coeffs = reverse b.coeffs

                    go :: ![a] ![a] ![a] -> ([a], [a]) | * a & - a & /a
                    go q a b
                      | length a < length b = (q, a)
                      | otherwise = go [q_: q] a` b
                          where (hd_b, tl_b) = (hd b, tl b)
                                q_ = hd a/hd_b
                                a` = subtractList q_ (tl a) tl_b

          // Вычитание двух списков коэффициентов.
          subtractList q a` b` = subtractListInternal a` b`
              where subtractListInternal [ax:a] [bx:b] = [(ax - bx * q):subtractListInternal a b]
                    subtractListInternal a [] = a
                    subtractListInternal [] [_] =
                      abort "In subtractList a b : degree b > degree a!"
