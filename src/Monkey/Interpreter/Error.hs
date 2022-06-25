module Monkey.Interpreter.Error where

import Data.Int (Int64)
import Data.Text (Text)
import Error.Diagnose
import Monkey.Interpreter.Value
import Monkey.Syntax (BinOp, Name)

type RuntimeError = Diagnostic Text

topLevelReturn :: Position -> RuntimeError
topLevelReturn pos = _topLevelReturn

unboundAssignment :: Position -> Name -> RuntimeError
unboundAssignment pos name = _unboundAssignment

keyNotInMap :: Position -> Value -> RuntimeError
keyNotInMap pos key = _keyNotInMap

nonMapAccess :: Position -> Value -> RuntimeError
nonMapAccess pos nonMap = _nonMapAccess

arrayOutOfBounds :: Position -> Int64 -> RuntimeError
arrayOutOfBounds pos ix = _arrayOutOfBounds

nonIntIndex :: Position -> Value -> RuntimeError
nonIntIndex pos nonInt = _nonIntIndex

invalidIndex :: Position -> Value -> Value -> RuntimeError
invalidIndex pos nonStructure ix = _invalidIndex

unboundVar :: Position -> Name -> RuntimeError
unboundVar pos name = _unboundVar

calledNonFunction :: Position -> Value -> RuntimeError
calledNonFunction pos nonFunction = _calledNonFunction

nonNumberNegation :: Position -> Value -> RuntimeError
nonNumberNegation pos nonNumber = _nonNumberNegation

nonBoolNot :: Position -> Value -> RuntimeError
nonBoolNot pos nonBool = _nonBoolNot

nonIntBitNot :: Position -> Value -> RuntimeError
nonIntBitNot pos nonInt = _nonIntBitNot

nonBoolCondition :: Position -> Value -> RuntimeError
nonBoolCondition pos nonBool = _nonBoolCondition

invalidBinOp :: Position -> BinOp -> Value -> Value -> RuntimeError
invalidBinOp pos op x y = _invalidBinOp

negativeShift :: Position -> BinOp -> Int64 -> RuntimeError
negativeShift pos op y = _negativeShift

invalidArity :: Position -> Int -> Int -> RuntimeError
invalidArity pos expected actual = _invalidArity
