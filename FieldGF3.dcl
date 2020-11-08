definition module FieldGF3

from StdOverloaded import class ==, class +, class -, class *, class /, class ~,
                          class fromInt, class toReal, class toString
import Gast.Gen

:: FieldGF3 = FieldGF3 Int

instance == FieldGF3

instance + FieldGF3

instance - FieldGF3

instance * FieldGF3

instance / FieldGF3

instance ~ FieldGF3

instance fromInt FieldGF3

instance toReal FieldGF3

instance toString FieldGF3

derive ggen FieldGF3
