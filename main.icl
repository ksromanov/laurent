/* Тестирование операций над полиномами Лорана с помощью Gast */
module main

import Laurent
import StdEnv

Start :: [String]
Start =
        map toString
        [ trim {expon = -2, coeffs = [0.0, 2.0, 1.0, -0.5, 0.0]},
          {expon = -3, coeffs = [1.0, 0.0, 2.0, 1.0, 1.0, 0.0, 6.0, ~2.2]},
          {expon = 0, coeffs = [1.0, 2.0]} + {expon = 1, coeffs = [1.0, 2.0]},
          {expon = 0, coeffs = [1.0, 2.0]} + {expon = 1, coeffs = [1.0, 2.0]},
          a * b,
          fromConst (evaluate a 2.0) + fromConst (evaluate b 2.0),
          fromConst (evaluate (a + b) 2.0),
          (fromConst 1.0) * (fromConst 1.0),
          (fromConst 3.0) * (fromConst 2.0),
          fromConst (evaluate a 2.0) * fromConst (evaluate b 2.0),
          fromConst (evaluate (a * b) 2.0)
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
          fromConst (evaluate ((fromCoeffs [2, 3, 4, 5, 6]) * (fromCoeffs [2, 3, 4, 5, 6])) 8),
          fromConst ((evaluate (fromCoeffs [2, 3, 4, 5, 6]) 8) * (evaluate (fromCoeffs [2, 3, 4, 5, 6]) 8))
        ]++ ["\n\n\n"] ++
        map toString
        [
            zx, rx
        ]
        where a = {expon = 0, coeffs = [1.0, 2.0]}
              b = {expon = 1, coeffs = [1.0, 2.0]}
              (zx, rx) = divmod (fromCoeffs [2, 3, 4, 5, 6]) (fromCoeffs [0, 0, 1])
