/* Тестирование операций над полиномами Лорана с помощью Gast */
module main

import Laurent
import StdEnv
import StdOverloaded

import Gast

import StdDebug

derive bimap []
derive genShow Laurent
derive gPrint Laurent

// Limit number of powers to the range [-100, 100]
ggen{|Laurent|} a state =
    map (\(expon, coeffs) -> {expon = expon, coeffs = coeffs})
        (diag2 expon coeffs)
    where coeffs = [[x \\ x <- xs] \\ xs <- ggen{|*->*|} a state]
          expon = [~100..100]

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

propertyMinus :: (Laurent Int) -> Bool
propertyMinus a = (a - a) == fromConst 0

Start :: [[String]]
Start = [ test propertyEq
        , test propertyConstEqFromCoeff
        , test propertyEvaluateAtPoint0
        , test propertyEvaluateAtPoint1
        , test propertyEvaluateMonomial
        , test propertyTrimNoZeroes
        , test propertyDoubleTrim
        , test propertyTrimEvaluate
        , test propertyMinus]
/*
        map toString
        [ trim {expon = -2, coeffs = [0.0, 2.0, 1.0, -0.5, 0.0]},
          {expon = -3, coeffs = [1.0, 0.0, 2.0, 1.0, 1.0, 0.0, 6.0, ~2.2]},
          {expon = 0, coeffs = [1.0, 2.0]} + {expon = 1, coeffs = [1.0, 2.0]},
          {expon = 0, coeffs = [1.0, 2.0]} + {expon = 1, coeffs = [1.0, 2.0]},
          a * b,
          fromConst (evaluateAtPoint a 2.0) + fromConst (evaluateAtPoint b 2.0),
          fromConst (evaluateAtPoint (a + b) 2.0),
          (fromConst 1.0) * (fromConst 1.0),
          (fromConst 3.0) * (fromConst 2.0),
          fromConst (evaluateAtPoint a 2.0) * fromConst (evaluateAtPoint b 2.0),
          fromConst (evaluateAtPoint (a * b) 2.0)
        ] ++ ["\n\n\n"] ++
        map toString
        [
          (fromConst 1) * (fromConst 1),
          (fromConst 3) * (fromConst 2),
          (fromCoeffs [2, 3, 4, 5, 6]) * (fromConst 2),
          (fromConst 2) * (fromCoeffs [2, 3, 4, 5, 6]),
          (fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [0, 1])
        ] ++ ["\n\n\n"] ++
        map toString
        [
          (fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [1, 0, 0]),
          (fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [0, 1, 0]),
          (fromCoeffs [0, 1, 0]) * (fromCoeffs [2, 3, 4, 5, 6]),
          (fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [0, 0, 1]),
          fromConst (evaluateAtPoint ((fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [2, 3, 4, 5, 6])) 8),
          fromConst ((evaluateAtPoint (fromCoeffs [2, 3, 4, 5, 6]) 8) * (evaluateAtPoint (fromCoeffs [2, 3, 4, 5, 6]) 8))
        ]++ ["\n\n\n"] ++
        map toString
        [
            zx, rx
        ]
        where a = {expon = 0, coeffs = [1.0, 2.0]}
              b = {expon = 1, coeffs = [1.0, 2.0]}
              (zx, rx) = divmod (fromCoeffs [2, 3, 4, 5, 6]) (fromCoeffs [0, 0, 1])
*/
