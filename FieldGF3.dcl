definition module FieldGF3

from StdOverloaded import class ==, class +, class -, class *, class /, class ~, class fromInt
import Gast.Gen

:: FieldGF3 = FieldGF3 Int

instance == FieldGF3

instance + FieldGF3

instance - FieldGF3

instance * FieldGF3

instance / FieldGF3

instance ~ FieldGF3

instance fromInt FieldGF3

derive ggen FieldGF3
