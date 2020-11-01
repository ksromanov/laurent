/* Тестирование операций над полиномами Лорана с помощью Gast */
module main

import Laurent
import StdEnv

import Gast

derive bimap []
derive ggen Laurent
derive genShow Laurent
derive gPrint Laurent

propertyEq :: (Laurent Int) -> Bool
propertyEq a = a == a

Start :: [String]
Start = test propertyEq
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
