module Monkey.Interpreter.Value where

import Data.HashTable.IO (CuckooHashTable)
import Data.Hashable (Hashable (..))
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Unique (Unique)
import Data.Vector.Mutable (IOVector)
import Monkey.Syntax (Expr, Name)

data PrimOp
  = Print
  | ReadLine
  deriving stock (Enum, Eq, Ord)

data Value
  = Unit
  | PrimOp PrimOp
  | Int Int64
  | Float Double
  | Char Char
  | Bool Bool
  | String Text
  | Array Unique (IOVector Value)
  | Map Unique (CuckooHashTable Value Value)
  | Closure Unique (Map Name Value) [Name] Expr

instance Eq Value where
  PrimOp op == PrimOp op' = op == op'
  Int x == Int x' = x == x'
  Float x == Float x' = x == x'
  Char c == Char c' = c == c'
  Bool b == Bool b' = b == b'
  String s == String s' = s == s'
  Array u _ == Array u' _ = u == u'
  Map u _ == Map u' _ = u == u'
  Closure u _ _ _ == Closure u' _ _ _ = u == u'
  _ == _ = False

instance Hashable Value where
  hashWithSalt s = \case
    Unit -> s `hashWithSalt` (0 :: Int)
    PrimOp op -> s `hashWithSalt` (1 :: Int) `hashWithSalt` fromEnum op
    Int x -> s `hashWithSalt` (2 :: Int) `hashWithSalt` x
    Float x -> s `hashWithSalt` (3 :: Int) `hashWithSalt` x
    Char c -> s `hashWithSalt` (4 :: Int) `hashWithSalt` c
    Bool b -> s `hashWithSalt` (5 :: Int) `hashWithSalt` b
    String t -> s `hashWithSalt` (6 :: Int) `hashWithSalt` t
    Array u _ -> s `hashWithSalt` (7 :: Int) `hashWithSalt` u
    Map u _ -> s `hashWithSalt` (8 :: Int) `hashWithSalt` u
    Closure u _ _ _ -> s `hashWithSalt` (9 :: Int) `hashWithSalt` u
