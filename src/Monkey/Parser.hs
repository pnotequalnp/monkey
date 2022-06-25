{-# OPTIONS_GHC -Wno-orphans #-}

module Monkey.Parser (
  parseFile,
) where

import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlphaNum)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Vector qualified as Vec
import Data.Void (Void)
import Effectful
import Effectful.Error.Static (Error, throwError)
import Error.Diagnose (Diagnostic)
import Error.Diagnose.Compat.Megaparsec (HasHints (..), errorDiagnosticFromBundle)
import Monkey.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

instance HasHints Void Text where
  hints = \case {}

parseFile :: Error (Diagnostic Text) :> es => FilePath -> Text -> Eff es [Statement]
parseFile fp = either (throwError . toDiagnostic) pure . parse (sc *> many statement <* eof) fp
  where
    toDiagnostic = errorDiagnosticFromBundle @Text Nothing "parse error" Nothing

statement :: Parser Statement
statement =
  choice
    [ binding <* semicolon
    , try assignment <* semicolon
    , controlFlow
    , Expr <$> expr <* semicolon
    ]
  where
    semicolon = some (symbol ";")
    binding = do
      _ <- symbol "let"
      (name, pos) <- lexeme (located identifier)
      _ <- symbol "="
      val <- lexeme expr
      pure (Binding pos name val)
    assignment = do
      open <- getSourcePos
      name <- lexeme identifier
      indices <- many do
        index <-
          Left <$> (symbol "." *> identifier)
            <|> Right <$> (between (symbol "[") (char ']') expr)
        close <- getSourcePos
        sc
        pure (index, close)
      op <- optional binOp
      _ <- symbol "="
      val <- lexeme expr
      let (path, ps) = unzip indices
          pos = toPosition open (last ps)
      pure (Assignment pos name path op val)
    controlFlow = do
      x <- while <|> ifElse
      _ <- optional (sc *> semicolon)
      pure (Expr x)
    binOp =
      choice
        [ Plus <$ string "+"
        , Minus <$ string "-"
        , Times <$ string "*"
        , Divide <$ string "/"
        , Mod <$ string "%"
        , And <$ string "&&"
        , Or <$ string "||"
        , BitAnd <$ string "&"
        , BitOr <$ string "|"
        , BitXor <$ string "^"
        , BitShiftLeft <$ string "<<"
        , BitShiftRight <$ string ">>"
        ]

expr :: Parser Expr
expr =
  makeExprParser
    (lexeme form)
    [ [pre Negate "-", pre Not "!", pre BitNot "~"]
    , [inL Times "*", inL Divide "/", inL Mod "%"]
    , [inL Plus "+", inL Minus "-"]
    , [inL BitShiftLeft "<<", inL BitShiftRight ">>"]
    , [inL LessThan "<", inL LessThanEqual "<=", inL GreaterThan ">", inL GreaterThanEqual ">="]
    , [inL Equal "==", inL NotEqual "!="]
    , [inL BitAnd "&"]
    , [inL BitXor "^"]
    , [inL BitOr "|"]
    , [inL And "&&"]
    , [inL Or "||"]
    ]
  where
    pre op sym = Prefix do
      (_, pos) <- lexeme (located (string sym)) <?> "unary operator"
      pure \x -> UnOp (pos <+> x.position) op x
    inL op sym = InfixL do
      _ <- symbol sym <?> "binary operator"
      pure \x y -> BinOp (x.position <+> y.position) op x y

form :: Parser Expr
form = do
  x <- atom
  xs <- many (call <|> index <|> access)
  pure (foldl' (&) x xs)
  where
    call = do
      args <- between (symbol "(") (char ')') (lexeme expr `sepBy` symbol ",") <?> "argument list"
      close <- getSourcePos
      pure \f -> Call (toPosition (getOpen f) close) f args
    index = do
      i <- between (symbol "[") (char ']') (lexeme expr) <?> "index"
      close <- getSourcePos
      pure \x -> Index (toPosition (getOpen x) close) x i
    access = do
      _ <- char '.' <?> "member access"
      name <- identifier
      close <- getSourcePos
      pure \x -> Access (toPosition (getOpen x) close) name x
    getOpen x =
      let Position (line, col) _ fp = x.position
       in SourcePos fp (mkPos line) (mkPos col)

atom :: Parser Expr
atom =
  choice
    [ ifElse <?> "conditional"
    , while <?> "loop"
    , return_ <?> "return"
    , function <?> "function"
    , try block <?> "block"
    , literal <?> "literal"
    , parens expr <?> "parenthesized expression"
    , var <?> "variable"
    ]

function :: Parser Expr
function = do
  open <- getSourcePos
  _ <- symbol "fn"
  _ <- symbol "("
  params <- (lexeme identifier `sepBy` symbol ",")
  body <- block
  _ <- char ')'
  close <- getSourcePos
  pure (Function (toPosition open close) params body)

block :: Parser Expr
block = do
  open <- getSourcePos
  _ <- symbol "{"
  stmts <- many statement
  val <- optional expr
  _ <- char '}'
  close <- getSourcePos
  pure (Block (toPosition open close) stmts val)

var :: Parser Expr
var = do
  (name, pos) <- located identifier
  pure (Var pos name)

return_ :: Parser Expr
return_ = do
  (x, pos) <- located do
    _ <- string "return"
    optional (sc *> expr)
  pure (Return pos x)

while :: Parser Expr
while = do
  open <- getSourcePos
  _ <- symbol "while"
  cond <- lexeme expr
  body <- block
  close <- getSourcePos
  pure (While (toPosition open close) cond body)

ifElse :: Parser Expr
ifElse = do
  open <- getSourcePos
  _ <- symbol "if"
  cond <- lexeme expr
  body <- block
  end <- optional do
    _ <- hidden sc
    _ <- symbol "else"
    block <|> ifElse
  close <- getSourcePos
  pure (If (toPosition open close) cond body end)

literal :: Parser Expr
literal =
  choice
    [ unitLiteral
    , boolLiteral
    , charLiteral
    , stringLiteral
    , arrayLiteral
    , mapLiteral
    , try floatLiteral
    , intLiteral
    ]

unitLiteral :: Parser Expr
unitLiteral = do
  (_, pos) <- located (string "()") <?> "()"
  pure (Lit pos UnitLit)

boolLiteral :: Parser Expr
boolLiteral = do
  (b, pos) <- located (False <$ string "false" <|> True <$ string "true") <?> "boolean"
  pure (Lit pos (BoolLit b))

intLiteral :: Parser Expr
intLiteral = do
  (x, pos) <- located (L.signed (pure ()) L.decimal) <?> "integer literal"
  pure (Lit pos (IntLit x))

floatLiteral :: Parser Expr
floatLiteral = do
  (x, pos) <- located (L.signed (pure ()) L.float) <?> "floating point literal"
  pure (Lit pos (FloatLit x))

charLiteral :: Parser Expr
charLiteral = do
  (c, pos) <- located (between (char '\'') (char '\'') L.charLiteral) <?> "char literal"
  pure (Lit pos (CharLit c))

stringLiteral :: Parser Expr
stringLiteral = do
  (s, pos) <- located (char '\"' *> manyTill L.charLiteral (char '\"')) <?> "string literal"
  pure (Lit pos (StringLit (pack s)))

arrayLiteral :: Parser Expr
arrayLiteral = label "array literal" do
  open <- getSourcePos
  _ <- symbol "["
  xs <- lexeme expr `sepEndBy` symbol ","
  _ <- char ']'
  close <- getSourcePos
  let l = ArrayLit (Vec.fromList xs)
  pure (Lit (toPosition open close) l)

mapLiteral :: Parser Expr
mapLiteral = do
  open <- getSourcePos
  _ <- symbol "{"
  entries <- entry `sepEndBy1` symbol ","
  _ <- char '}'
  close <- getSourcePos
  pure (Lit (toPosition open close) (MapLit entries))
  where
    entry = do
      key <- lexeme expr
      _ <- symbol ":"
      val <- lexeme expr
      pure (key, val)

identifier :: Parser Text
identifier = label "identifier" do
  h <- letterChar
  t <- takeWhileP Nothing (\c -> c == '_' || isAlphaNum c)
  let n = h `T.cons` t
  guard (n `notElem` keywords)
  pure n

keywords :: [Text]
keywords =
  [ "let"
  , "true"
  , "false"
  , "while"
  , "if"
  , "else"
  , "return"
  ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sc :: Parser ()
sc = L.space space1 lineComment blockComment

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

located :: Parser a -> Parser (a, Position)
located p = do
  open <- getSourcePos
  x <- p
  close <- getSourcePos
  pure (x, toPosition open close)

toPosition :: SourcePos -> SourcePos -> Position
toPosition
  SourcePos {sourceName = file, sourceLine = startLine, sourceColumn = startCol}
  SourcePos {sourceLine = endLine, sourceColumn = endCol} =
    Position
      { begin = (unPos startLine, unPos startCol)
      , end = (unPos endLine, unPos endCol)
      , file
      }
