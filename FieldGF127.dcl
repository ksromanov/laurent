definition module FieldGF127

from StdOverloaded import class ==, class +, class -, class *, class /, class ~,
                          class fromInt, class toReal, class toString
import Gast.Gen

:: FieldGF127 = FieldGF127 Int

instance == FieldGF127

instance + FieldGF127

instance - FieldGF127

instance * FieldGF127

instance / FieldGF127

instance ~ FieldGF127

instance fromInt FieldGF127

instance toReal FieldGF127

instance toString FieldGF127

derive ggen FieldGF127
