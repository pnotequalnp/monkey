module Monkey.Interpreter.Error where

import Data.Text (Text)
import Data.Text qualified as T
import Error.Diagnose
import Monkey.Interpreter.Value
import Monkey.Syntax (BinOp (..), Name)
import Data.Bool (bool)

type RuntimeError = Diagnostic Text

topLevelReturn :: Position -> RuntimeError
topLevelReturn pos = def `addReport` err Nothing msg [(pos, This msg)] hints
  where
    msg = "top-level `return`"
    hints = ["`return` can only appear in a function body"]

unboundAssignment :: Position -> Name -> RuntimeError
unboundAssignment pos name = def `addReport` err Nothing msg [(pos, This usage)] []
  where
    msg = "unbound variable in assignment"
    usage = "unbound variable " <> quoted name

keyNotInMap :: Position -> Value -> RuntimeError
keyNotInMap pos key = def `addReport` err Nothing msg [(pos, This usage)] []
  where
    msg = "key not found"
    keyText = case key of
      Unit -> "`()`"
      Int x -> quoted (showt x)
      Float x -> quoted (showt x)
      Char c -> quoted (T.singleton c)
      Bool b -> quoted (bool "false" "true" b)
      String s -> quoted s
      _ -> ""
    usage = T.concat ["key ", keyText, "not found"]

nonMapAccess :: Position -> Value -> RuntimeError
nonMapAccess pos nonMap = def `addReport` err Nothing msg [(pos, This usage)] hints
  where
    msg = "invalid access"
    usage = "cannot access object of type " <> quotedType nonMap
    hints = ["only maps can be accessed"]

arrayOutOfBounds :: Position -> RuntimeError
arrayOutOfBounds pos = def `addReport` err Nothing msg [(pos, This msg)] []
  where
    msg = "index out of bounds"

nonIntIndex :: Position -> Value -> RuntimeError
nonIntIndex pos nonInt = def `addReport` err Nothing msg [(pos, This usage)] hints
  where
    msg = "invalid index"
    usage = "cannot index array with object of type " <> quotedType nonInt
    hints = ["arrays can only be indexed by `int`s"]

invalidIndex :: Position -> Value -> Value -> RuntimeError
invalidIndex pos nonStructure ix = def `addReport` err Nothing msg [(pos, This usage)] hints
  where
    msg = "invalid index"
    usage = T.concat ["cannot index ", quotedType nonStructure, " with object of type ", quotedType ix]
    hints = ["only `map`s and `array`s can be indexed"]

unboundVar :: Position -> Name -> RuntimeError
unboundVar pos name = def `addReport` err Nothing msg [(pos, This usage)] []
  where
    msg = "unbound variable"
    usage = "unbound variable " <> quoted name

calledNonFunction :: Position -> Value -> RuntimeError
calledNonFunction pos nonFunction = def `addReport` err Nothing msg [(pos, This usage)] hints
  where
    msg = "invalid function call"
    usage = T.concat ["object of type ", quotedType nonFunction, " called"]
    hints = ["only `closure`s and `primop`s can be called"]

nonNumberNegation :: Position -> Value -> RuntimeError
nonNumberNegation pos nonNumber = def `addReport` err Nothing msg [(pos, This usage)] hints
  where
    msg = "invalid negation"
    usage = T.concat ["object of type ", quotedType nonNumber, " negated"]
    hints = ["only `int`s and `float`s can be negated"]

nonBoolNot :: Position -> Value -> RuntimeError
nonBoolNot pos nonBool = def `addReport` err Nothing msg [(pos, This usage)] hints
  where
    msg = "invalid logical negation"
    usage =  T.concat ["object of type ", quotedType nonBool, " logically negated"]
    hints = ["only `bool`s can be logically negated"]

nonIntBitNot :: Position -> Value -> RuntimeError
nonIntBitNot pos nonInt = def `addReport` err Nothing msg [(pos, This usage)] hints
  where
    msg = "invalid bitwise negation"
    usage =  T.concat ["object of type ", quotedType nonInt, " bitwise negated"]
    hints = ["only `int`s can be bitwise negated"]

nonBoolCondition :: Position -> Value -> RuntimeError
nonBoolCondition pos nonBool = def `addReport` err Nothing msg [(pos, This usage)] hints
  where
    msg = "invalid condition"
    usage = "condition has type " <> quotedType nonBool
    hints = ["conditions must be `bool`s"]

invalidBinOp :: Position -> BinOp -> Value -> Value -> RuntimeError
invalidBinOp pos op x y = def `addReport` err Nothing msg [(pos, This usage)] []
  where
    msg = "incompatible operands of binary operator"
    opText = quoted case op of
      Plus -> "+"
      Minus -> "-"
      Times -> "*"
      Divide -> "/"
      Mod -> "%"
      Equal -> "=="
      NotEqual -> "!="
      LessThan -> "<"
      LessThanEqual -> "<="
      GreaterThanEqual -> ">="
      GreaterThan -> ">"
      And -> "&&"
      Or -> "||"
      BitAnd -> "&"
      BitOr -> "|"
      BitXor -> "^"
      BitShiftLeft -> "<<"
      BitShiftRight -> ">>"
    usage = T.concat ["incompatible operand types ", quotedType x, " and ", quotedType y, " for operator ", opText]

negativeShift :: Position -> RuntimeError
negativeShift pos = def `addReport` err Nothing msg [(pos, This msg)] hints
  where
    msg = "negative bitwise shift"
    hints = ["shift amount must be non-negative"]

invalidArity :: Position -> Int -> Int -> RuntimeError
invalidArity pos expected actual = def `addReport` err Nothing msg [(pos, This usage)] []
  where
    msg = "invalid function arity"
    usage = T.concat ["function expecting ", showt expected, " argument", plural, ", but called with ", showt actual ]
    plural = case expected of
      1 -> ""
      _ -> "s"

invalidLength :: Position -> Value -> RuntimeError
invalidLength pos nonStructure = def `addReport` err Nothing msg [(pos, This usage)] hints
  where
    msg = "invalid length"
    usage = "attempt to get length of object of type " <> quotedType nonStructure
    hints = ["only `array`s have a length"]

showt :: Show a => a -> Text
showt = T.pack . show

quoted :: Text -> Text
quoted t = T.concat ["`", t, "`"]

quotedType :: Value -> Text
quotedType = \case
  Unit -> "`unit`"
  PrimOp _ -> "`primop`"
  Int _ -> "`int`"
  Float _ -> "`float`"
  Char _ -> "`char`"
  Bool _ -> "`bool`"
  String _ -> "`string`"
  Array _ _ -> "`array`"
  Map _ _ -> "`map`"
  Closure _ _ _ _ -> "`closure`"
