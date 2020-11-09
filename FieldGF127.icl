implementation module FieldGF127

import StdDebug

import StdArray
import StdBool
import StdInt
import StdList
import StdMisc
import StdOverloaded
import StdGeneric
import StdEnv

import Gast

:: FieldGF127 = FieldGF127 Int

normalize :: FieldGF127 -> FieldGF127
normalize (FieldGF127 a) = FieldGF127 (((a rem 127) + 127) rem 127)

instance == FieldGF127 where
    (==) (FieldGF127 a) (FieldGF127 b) = ((a + 127) rem 127) == ((b + 127) rem 127)

instance + FieldGF127 where
    (+) (FieldGF127 a) (FieldGF127 b) = FieldGF127 ((a + b + 127) rem 127)

instance - FieldGF127 where
    (-) (FieldGF127 a) (FieldGF127 b) = FieldGF127 ((a - b + 127) rem 127)

instance * FieldGF127 where
    (*) (FieldGF127 a) (FieldGF127 b) = FieldGF127 ((a * b) rem 127)

inverse :: FieldGF127 -> FieldGF127
inverse a = /*trace(foldl (+++) "" (map (\i -> "  " +++ toString i) [x \\ x <-: inverses])) */ FieldGF127 inverses.[a`]
    where (FieldGF127 a`) = normalize a   
          inverses :: {#Int}
          inverses = { find_inverse 0 x \\ x <- [0..126] }
          find_inverse inv x
            | x == 0 = 0 // dummy value
            | (((inv * x) rem 127) + 127) rem 127 == 1 = inv
            | otherwise = find_inverse (inv + 1) x

instance / FieldGF127 where
    (/) _ (FieldGF127 0) = abort "division by 0"
    (/) a b = a * inverse b

instance ~ FieldGF127 where
    ~ (FieldGF127 a) = FieldGF127 ((127 - a) rem 127)

instance fromInt FieldGF127 where
    fromInt i = FieldGF127 (((i rem 127) + 127) rem 127)

instance toReal FieldGF127 where
    toReal (FieldGF127 a) = toReal a

instance toString FieldGF127 where
    toString a = toString a`
        where (FieldGF127 a`) = normalize a

ggen{|FieldGF127|} state = map fromInt [0 .. 126]
