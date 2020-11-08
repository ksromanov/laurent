/* Тестирование операций над полиномами Лорана с помощью Gast */
module main

import Laurent
import StdEnv
import StdOverloaded

import Gast

import StdDebug

import FieldGF2

derive bimap []
derive genShow Laurent
derive gPrint Laurent

// Limit number of powers to the range [-100, 100]
// Этот генератор ужасно работает с Real
ggen{|Laurent|} a state =
    map (\(expon, coeffs) -> {expon = expon, coeffs = coeffs})
        (diag2 expon coeffs)
    where coeffs = ggen{|*->*|} a state
          expon = [~5..5]

propertyEq :: (Laurent Int) -> Bool
propertyEq a = a == a

// Свойства генераторов
propertyConstEqFromCoeff :: Int -> Bool
propertyConstEqFromCoeff c = fromConst c == fromCoeffs [c] &&
                             fromConst c == fromShiftCoeffs 0 [c]

// Вычисление в точке
propertyEvaluateAtPoint0 :: (Laurent Int) -> Bool
propertyEvaluateAtPoint0 a
    | a.expon == 0 && a.coeffs <> [] =
        evaluateAtPoint a 0 == hd a.coeffs
    | a.expon > 0 && a.expon < 1000 =
        evaluateAtPoint a 0 == 0
    | otherwise = True

propertyEvaluateAtPoint1 :: (Laurent Int) -> Bool
propertyEvaluateAtPoint1 a
    | a.expon > -1000 && a.expon < 1000 =
        evaluateAtPoint a 1 == foldl (+) 0 a.coeffs
    | otherwise = True

propertyEvaluateMonomial :: Int Real Real -> Bool
propertyEvaluateMonomial n c x
    | expon <= 0 && x == 0.0 = True
    | isFinite x && isFinite c = evaluateAtPoint { expon = expon, coeffs = [c] } x == c * x^(fromInt expon)
    | otherwise = True // отсекаем NaN и прочие глупости
    where expon = (n rem 200) - 100

// Проверка trim
propertyTrimNoZeroes :: (Laurent Int) -> Bool
propertyTrimNoZeroes a
    | a`.coeffs == [] = True
    | otherwise =
        hd a`.coeffs <> 0 && hd (reverse a`.coeffs) <> 0
    where a` = trim a

propertyDoubleTrim :: (Laurent Int) -> Bool
propertyDoubleTrim a = trim (trim a) == trim a

propertyTrimEvaluate :: (Laurent Int) Int -> Bool
propertyTrimEvaluate a x
    | x == 0 && a.expon < 0 = True
    | otherwise = evaluateAtPoint a x == evaluateAtPoint a x

// Проверка сложения и вычитания
propertyMinus :: (Laurent Int) -> Bool
propertyMinus a = (a - a) == fromConst 0

propertyMinusEval :: (Laurent FieldGF2) (Laurent FieldGF2) FieldGF2 -> Bool
propertyMinusEval a b x
    | (a.expon < 0 || b.expon < 0) && x == FieldGF2 False = True
    | otherwise = evaluateAtPoint a x - evaluateAtPoint b x == evaluateAtPoint (a - b) x

propertyPlusEval :: (Laurent FieldGF2) (Laurent FieldGF2) FieldGF2 -> Bool
propertyPlusEval a b x
    | (a.expon < 0 || b.expon < 0) && x == FieldGF2 False = True
    | otherwise =
        evaluateAtPoint a x + evaluateAtPoint b x == evaluateAtPoint (a - b) x

propertyMultiplyEval :: (Laurent FieldGF2) (Laurent FieldGF2) FieldGF2 -> Bool
propertyMultiplyEval a b x
    | (a.expon < 0 || b.expon < 0) && x == FieldGF2 False = True
    | otherwise =
        evaluateAtPoint a x * evaluateAtPoint b x == evaluateAtPoint (a * b) x

print a = "[" +++ toString a.expon +++ ": "
    +++ foldl (+++) "" (map (\x -> " " +++ toString x) a.coeffs)
    +++ "]"

propertyGF2PlusMinus :: FieldGF2 FieldGF2 -> Bool
propertyGF2PlusMinus a b =
       a + b == b + a
    && a + b - b == a 
    && a + b == b + a

propertyGF2Mult :: FieldGF2 FieldGF2 -> Bool
propertyGF2Mult a b =
    a * b == b * a

propertyGF2Div :: FieldGF2 FieldGF2 -> Bool
propertyGF2Div a b
    | b == (FieldGF2 False) = True
    | otherwise = (a / b) * b == a

derive ggen FieldGF2
derive genShow FieldGF2
derive gPrint FieldGF2

Start :: [[String]]
Start = [ test propertyGF2PlusMinus
        , test propertyGF2Mult
        , test propertyGF2Div
        , test propertyEq
        , test propertyConstEqFromCoeff
        , test propertyEvaluateAtPoint0
        , test propertyEvaluateAtPoint1
        , test propertyEvaluateMonomial
        , test propertyTrimNoZeroes
        , test propertyDoubleTrim
        , test propertyTrimEvaluate
        , test propertyMinus
        , test propertyMinusEval
        , test propertyPlusEval
        , test propertyMultiplyEval]
