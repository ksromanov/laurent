/* Тестирование операций над полиномами Лорана с помощью Gast */
module main

import Laurent
import StdEnv
import StdOverloaded

import Gast

import StdDebug

import FieldGF2
import FieldGF3
import FieldGF127

derive bimap []
derive genShow Laurent
derive gPrint Laurent

// Вспомогательные значения a
zero :: a | fromInt a
zero = fromInt 0

one :: a | fromInt a
one = fromInt 1

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

propertyMinusEvalGF2 :: (Laurent FieldGF2) (Laurent FieldGF2) FieldGF2 -> Bool
propertyMinusEvalGF2 a b x
    | (a.expon < 0 || b.expon < 0) && x == zero = True
    | otherwise = evaluateAtPoint a x - evaluateAtPoint b x == evaluateAtPoint (a - b) x

propertyPlusEvalGF2 :: (Laurent FieldGF2) (Laurent FieldGF2) FieldGF2 -> Bool
propertyPlusEvalGF2 a b x
    | (a.expon < 0 || b.expon < 0) && x == zero = True
    | otherwise =
        evaluateAtPoint a x + evaluateAtPoint b x == evaluateAtPoint (a + b) x

propertyMultiplyEvalGF2 :: (Laurent FieldGF2) (Laurent FieldGF2) FieldGF2 -> Bool
propertyMultiplyEvalGF2 a b x
    | (a.expon < 0 || b.expon < 0) && x == zero = True
    | otherwise =
        evaluateAtPoint a x * evaluateAtPoint b x == evaluateAtPoint (a * b) x

// Проверка сложения и вычитания на поле GF3
propertyMinusEvalGF3 :: (Laurent FieldGF3) (Laurent FieldGF3) FieldGF3 -> Bool
propertyMinusEvalGF3 a b x
    | (a.expon < 0 || b.expon < 0) && x == zero = True
    | otherwise = evaluateAtPoint a x - evaluateAtPoint b x == evaluateAtPoint (a - b) x

propertyPlusEvalGF3 :: (Laurent FieldGF3) (Laurent FieldGF3) FieldGF3 -> Bool
propertyPlusEvalGF3 a b x
    | (a.expon < 0 || b.expon < 0) && x == zero = True
    | otherwise =
        evaluateAtPoint a x + evaluateAtPoint b x == evaluateAtPoint (a + b) x

propertyMultiplyEvalGF3 :: (Laurent FieldGF3) (Laurent FieldGF3) FieldGF3 -> Bool
propertyMultiplyEvalGF3 a b x
    | (a.expon < 0 || b.expon < 0) && x == zero = True
    | otherwise =
        evaluateAtPoint a x * evaluateAtPoint b x == evaluateAtPoint (a * b) x

// Проверка сложения и вычитания на поле GF127
propertyMinusEvalGF127 :: (Laurent FieldGF127) (Laurent FieldGF127) FieldGF127 -> Bool
propertyMinusEvalGF127 a b x
    | (a.expon < 0 || b.expon < 0) && x == zero = True
    | otherwise = evaluateAtPoint a x - evaluateAtPoint b x == evaluateAtPoint (a - b) x

propertyPlusEvalGF127 :: (Laurent FieldGF127) (Laurent FieldGF127) FieldGF127 -> Bool
propertyPlusEvalGF127 a b x
    | (a.expon < 0 || b.expon < 0) && x == zero = True
    | otherwise =
        evaluateAtPoint a x + evaluateAtPoint b x == evaluateAtPoint (a + b) x

propertyMultiplyEvalGF127 :: (Laurent FieldGF127) (Laurent FieldGF127) FieldGF127 -> Bool
propertyMultiplyEvalGF127 a b x
    | (a.expon < 0 || b.expon < 0) && x == zero = True
    | otherwise =
        evaluateAtPoint a x * evaluateAtPoint b x == evaluateAtPoint (a * b) x

print a = "[" +++ toString a.expon +++ ": "
    +++ foldl (+++) "" (map (\x -> " " +++ toString x) a.coeffs)
    +++ "]"

propertyGF2PlusMinus :: FieldGF2 FieldGF2 -> Bool
propertyGF2PlusMinus a b =
       a + b == b + a
    && a + b - b == a
    && a + zero == a

propertyGF2Mult :: FieldGF2 FieldGF2 -> Bool
propertyGF2Mult a b =
       a * b == b * a
    && a * zero == zero
    && a * one == a

propertyGF2Mult01 :: FieldGF2 -> Bool
propertyGF2Mult01 a =
       a * zero == zero
    && a * one == a

propertyGF2Div :: FieldGF2 FieldGF2 -> Bool
propertyGF2Div a b
    | b == zero = True
    | otherwise = (a / b) * b == a

derive genShow FieldGF2
derive gPrint FieldGF2

derive genShow FieldGF3
derive gPrint FieldGF3

derive genShow FieldGF127
derive gPrint FieldGF127

propertyGF3PlusMinus :: FieldGF3 FieldGF3 -> Bool
propertyGF3PlusMinus a b =
       a + b == b + a
    && a + b - b == a
    && a - b == ~(b - a)

propertyGF3Mult :: FieldGF3 FieldGF3 -> Bool
propertyGF3Mult a b =
       a * b == b * a

propertyGF3Mult01 :: FieldGF3 -> Bool
propertyGF3Mult01 a =
       a * zero == zero
    && a * one  == a

propertyGF3Div :: FieldGF3 FieldGF3 -> Bool
propertyGF3Div a b
    | b == zero = True
    | otherwise = (a / b) * b == a

propertyGF127PlusMinus :: FieldGF127 FieldGF127 -> Bool
propertyGF127PlusMinus a b =
       a + b == b + a
    && a + b - b == a
    && a - b == ~(b - a)

propertyGF127Mult :: FieldGF127 FieldGF127 -> Bool
propertyGF127Mult a b =
       a * b == b * a

propertyGF127Mult01 :: FieldGF127 -> Bool
propertyGF127Mult01 a =
       a * zero == zero
    && a * one  == a

propertyGF127Div :: FieldGF127 FieldGF127 -> Bool
propertyGF127Div a b
    | b == zero = True
    | otherwise = (a / b) * b == a

Start :: [[String]]
Start = [ test propertyGF2PlusMinus
        , test propertyGF2Mult
        , test propertyGF2Mult01
        , test propertyGF2Div
        , test propertyGF3PlusMinus
        , test propertyGF3Mult
        , test propertyGF3Mult01
        , test propertyGF3Div
        , testn 20000 propertyGF127PlusMinus
        , testn 20000 propertyGF127Mult
        , test propertyGF127Mult01
        , testn 20000 propertyGF127Div
        , test propertyEq
        , test propertyConstEqFromCoeff
        , test propertyEvaluateAtPoint0
        , test propertyEvaluateAtPoint1
        , test propertyEvaluateMonomial
        , test propertyTrimNoZeroes
        , test propertyDoubleTrim
        , test propertyTrimEvaluate
        , test propertyMinus
        , test propertyMinusEvalGF2
        , test propertyPlusEvalGF2
        , test propertyMultiplyEvalGF2
        , test propertyMinusEvalGF3
        , test propertyPlusEvalGF3
        , test propertyMultiplyEvalGF3
        , testn 20000 propertyMinusEvalGF127
        , testn 20000 propertyPlusEvalGF127
        , testn 20000 propertyMultiplyEvalGF127]
