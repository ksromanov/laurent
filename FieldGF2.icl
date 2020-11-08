implementation module FieldGF2

import StdBool
import StdInt
import StdMisc
import StdOverloaded

import Gast

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

instance ~ FieldGF2 where
    ~ a = a

instance fromInt FieldGF2 where
    fromInt i = FieldGF2 (not (i == 0))

instance toReal FieldGF2 where
    toReal (FieldGF2  True) = 1.0
    toReal (FieldGF2 False) = 0.0

instance toString FieldGF2 where
    toString (FieldGF2  True) = "1"
    toString (FieldGF2 False) = "0"

derive bimap []
derive ggen FieldGF2
