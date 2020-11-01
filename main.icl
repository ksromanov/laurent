/* Тестирование операций над полиномами Лорана с помощью Gast */
module main

import Laurent
import StdEnv
import StdOverloaded

import Gast

derive bimap []
derive ggen Laurent
derive genShow Laurent
derive gPrint Laurent

propertyEq :: (Laurent Int) -> Bool
propertyEq a = a == a

// Свойства генераторов
propertyConstEqFromCoeff :: Int -> Bool
propertyConstEqFromCoeff c = fromConst c == fromCoeffs [c] &&
                             fromConst c == fromShiftCoeffs 0 [c]

// Вычисление в точке
propertyEvaluateAtPoint0 :: (Laurent Int) -> Bool
propertyEvaluateAtPoint0 a
    | a.expon > 0 && a.expon < 1000 =
        evaluateAtPoint a 0 == 0
    | otherwise = True

propertyEvaluateAtPoint1 :: (Laurent Int) -> Bool
propertyEvaluateAtPoint1 a
    | a.expon > -1000 && a.expon < 1000 =
        evaluateAtPoint a 1 == foldl (+) 0 a.coeffs
    | otherwise = True

propertyMinus :: (Laurent Int) -> Bool
propertyMinus a = (a - a) == fromConst 0

Start :: [[String]]
Start = [ test propertyEq
        , test propertyConstEqFromCoeff
        , test propertyEvaluateAtPoint0
        , test propertyEvaluateAtPoint1
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
