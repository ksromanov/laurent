implementation module FieldGF2

import StdBool
import StdInt
import StdMisc
import StdOverloaded

:: FieldGF2 = FieldGF2 Bool

instance == FieldGF2 where
    (==) (FieldGF2 a) (FieldGF2 b) = a == b

instance + FieldGF2 where
    (+) (FieldGF2 a) (FieldGF2 b) = FieldGF2 ((a && (not b)) || (b && (not a)))

instance - FieldGF2 where
    (-) a b = a + b // For GF2 + and - are both xor

instance * FieldGF2 where
    (*) (FieldGF2 a) (FieldGF2 b) = FieldGF2 (a && b)

instance / FieldGF2 where
    (/) a (FieldGF2  True) = a
    (/) a (FieldGF2 False) = abort "division by 0"

instance fromInt FieldGF2 where
    fromInt i = FieldGF2 (not (i == 0))
