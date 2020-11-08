implementation module FieldGF3

import StdBool
import StdInt
import StdList
import StdMisc
import StdOverloaded
import StdGeneric

import Gast

:: FieldGF3 = FieldGF3 Int

normalize :: FieldGF3 -> FieldGF3
normalize (FieldGF3 a) = FieldGF3 (((a rem 3) + 3) rem 3)

instance == FieldGF3 where
    (==) (FieldGF3 a) (FieldGF3 b) = (a rem 3) == (b rem 3)

instance + FieldGF3 where
    (+) (FieldGF3 a) (FieldGF3 b) = FieldGF3 ((a + b + 3) rem 3)

instance - FieldGF3 where
    (-) (FieldGF3 a) (FieldGF3 b) = FieldGF3 ((a - b + 3) rem 3)

instance * FieldGF3 where
    (*) (FieldGF3 a) (FieldGF3 b) = FieldGF3 ((a * b) rem 3)

instance / FieldGF3 where
    (/) _ (FieldGF3 0) = abort "division by 0"
    (/) a b = FieldGF3 (3 - ((2*a`/b`) rem 3))
        where (FieldGF3 a`) = normalize a
              (FieldGF3 b`) = normalize b

instance ~ FieldGF3 where
    ~ (FieldGF3 a) = FieldGF3 ((3 - a) rem 3)

instance fromInt FieldGF3 where
    fromInt i = FieldGF3 (((i rem 3) + 3) rem 3)

ggen{|FieldGF3|} state = map fromInt (ggen{|*|} state)
