module Monkey.Syntax (
  Statement (..),
  UnOp (..),
  BinOp (..),
  Name,
  Expr (..),
  Lit (..),
  Position (..),
  (<+>),
) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Error.Diagnose (Position (..))
import GHC.Records (HasField (..))

data Statement
  = Binding Position Name Expr
  | Assignment Position Name [Either Name Expr] (Maybe BinOp) Expr
  | Expr Expr
  deriving stock (Show)

instance HasField "position" Statement Position where
  getField = \case
    Binding p _ _ -> p
    Assignment p _ _ _ _ -> p
    Expr x -> x.position

data UnOp
  = Negate
  | Not
  | BitNot
  deriving stock (Show)

data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Equal
  | NotEqual
  | LessThan
  | LessThanEqual
  | GreaterThanEqual
  | GreaterThan
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor
  | BitShiftLeft
  | BitShiftRight
  deriving stock (Show)

type Name = Text

data Lit
  = UnitLit
  | IntLit Int64
  | FloatLit Double
  | CharLit Char
  | BoolLit Bool
  | StringLit Text
  | ArrayLit (Vector Expr)
  | MapLit [(Expr, Expr)]
  deriving stock (Show)

data Expr
  = Lit Position Lit
  | Var Position Name
  | Index Position Expr Expr
  | Access Position Name Expr
  | Call Position Expr [Expr]
  | UnOp Position UnOp Expr
  | BinOp Position BinOp Expr Expr
  | Block Position [Statement] (Maybe Expr)
  | While Position Expr Expr
  | If Position Expr Expr (Maybe Expr)
  | Return Position (Maybe Expr)
  | Function Position [Name] Expr
  deriving stock (Show)

instance HasField "position" Expr Position where
  getField = \case
    Lit p _ -> p
    Var p _ -> p
    Index p _ _ -> p
    Access p _ _ -> p
    Call p _ _ -> p
    UnOp p _ _ -> p
    BinOp p _ _ _ -> p
    Block p _ _ -> p
    While p _ _ -> p
    If p _ _ _ -> p
    Return p _ -> p
    Function p _ _ -> p

(<+>) :: Position -> Position -> Position
Position {begin, file} <+> Position {end} = Position {begin, end, file}
