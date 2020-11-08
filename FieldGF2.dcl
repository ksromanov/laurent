definition module FieldGF2

from StdOverloaded import class ==, class +, class -, class *, class /, class ~,
                          class fromInt, class toReal, class toString
import Gast.Gen

:: FieldGF2 = FieldGF2 Bool

instance == FieldGF2

instance + FieldGF2

instance - FieldGF2

instance * FieldGF2

instance / FieldGF2

instance ~ FieldGF2

instance fromInt FieldGF2

instance toReal FieldGF2

instance toString FieldGF2

derive ggen FieldGF2
