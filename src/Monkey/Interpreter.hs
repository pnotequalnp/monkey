module Monkey.Interpreter (
  interpret,
) where

import Control.Applicative ((<|>))
import Control.Monad (unless, void, when, (<=<))
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Bool (bool)
import Data.Foldable (foldl', for_, traverse_)
import Data.Functor ((<&>))
import Data.HashTable.IO (CuckooHashTable)
import Data.HashTable.IO qualified as HT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Data.Unique (newUnique)
import Data.Vector qualified as Vec
import Data.Vector.Mutable (IOVector)
import Data.Vector.Mutable qualified as MVec
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError, tryError)
import Effectful.Prim (Prim)
import Effectful.Reader.Static (Reader, local, runReader)
import Effectful.State.Static.Local (State, evalState, get, gets, modify, put)
import Monkey.Interpreter.Error
import Monkey.Interpreter.Value
import Monkey.Syntax

type Stack = [Frame]
type Frame = Map Name (IORef Value)
type Callstack = [Position]
type RuntimeErrors = Error RuntimeError
type Return = Error (Value, Position)
type Eval = [IOE, Prim, RuntimeErrors, State Stack, Reader Callstack, Return]

interpret :: [IOE, Prim, RuntimeErrors] :>> es => [Statement] -> Eff es ()
interpret prog = do
  primOps <- makePrimOps
  runReader @Callstack [] . evalState [primOps] $ do
    runErrorNoCallStack @(Value, Position) (traverse_ exec prog) >>= \case
      Left (_, pos) -> throwError (topLevelReturn pos)
      Right _ -> pure ()

exec :: Eval :>> es => Statement -> Eff es ()
exec = \case
  Binding pos name x -> execBinding pos name x
  Assignment pos name path op x -> execAssignment pos name path op x
  Expr x -> void (eval x)

execBinding :: Eval :>> es => Position -> Name -> Expr -> Eff es ()
execBinding _ name x = mdo
  val <- case x of
    Function _ _ _ -> do
      pushFrame (Map.singleton name ref)
      f <- eval x
      dropFrame
      pure f
    _ -> eval x
  ref <- liftIO (newIORef val)
  modify \case
    [] -> []
    frame : frames -> Map.insert name ref frame : frames

execAssignment :: Eval :>> es => Position -> Name -> [Either Name Expr] -> Maybe BinOp -> Expr -> Eff es ()
execAssignment pos name path op x = do
  ref <- getVar name >>= maybe (throwError (unboundAssignment pos name)) pure
  val <- liftIO (readIORef ref)
  case nonEmpty path of
    Nothing -> do
      val' <- reval val
      liftIO (writeIORef ref val')
    Just ixs -> mutate ixs val
  where
    reval prev = case op of
      Nothing -> eval x
      Just op' -> do
        val <- eval x
        binOp (pos <+> x.position) op' prev val
    mutate (ix :| indices) val = case nonEmpty indices of
      Nothing | Left field <- ix ->
        case val of
          Map _ m -> modifyMap pos m (String field) reval
          nonMap -> throwError (nonMapAccess pos nonMap)
      Nothing | Right ix' <- ix -> do
        i <- eval ix'
        case val of
          Array _ v -> modifyArray pos v i reval
          Map _ m -> modifyMap pos m i reval
          nonStructure -> throwError (invalidIndex pos nonStructure i)
      Just ixs -> do
        val' <- case ix of
          Left field -> access pos field val
          Right i -> eval i >>= index pos val
        mutate ixs val'

eval :: Eval :>> es => Expr -> Eff es Value
eval = \case
  Lit pos lit -> evalLit pos lit
  Var pos name -> evalVar pos name
  Index pos x ix -> evalIndex pos x ix
  Access pos field x -> evalAccess pos field x
  Call pos f args -> evalCall pos f args
  UnOp pos op x -> evalUnOp pos op x
  BinOp pos op x y -> evalBinOp pos op x y
  Block pos stmts x -> evalBlock pos stmts x
  While pos cond body -> evalWhile pos cond body
  If pos cond x y -> evalIf pos cond x y
  Return pos x -> evalReturn pos x
  Function pos params body -> evalFunction pos params body

evalLit :: Eval :>> es => Position -> Lit -> Eff es Value
evalLit _ = \case
  UnitLit -> pure Unit
  IntLit x -> pure (Int x)
  FloatLit x -> pure (Float x)
  CharLit c -> pure (Char c)
  BoolLit b -> pure (Bool b)
  StringLit s -> pure (String s)
  ArrayLit xs -> do
    vals <- traverse eval xs
    u <- liftIO newUnique
    Array u <$> Vec.unsafeThaw vals
  MapLit xs -> do
    t <- liftIO (HT.newSized (length xs))
    for_ xs \(k, v) -> do
      key <- eval k
      val <- eval v
      liftIO (HT.insert t key val)
    u <- liftIO newUnique
    pure (Map u t)

evalVar :: Eval :>> es => Position -> Name -> Eff es Value
evalVar pos name =
  getVar name >>= \case
    Nothing -> throwError (unboundVar pos name)
    Just ref -> liftIO (readIORef ref)

evalIndex :: Eval :>> es => Position -> Expr -> Expr -> Eff es Value
evalIndex pos x ix = do
  val <- eval x
  i <- eval ix
  index pos val i

index :: Eval :>> es => Position -> Value -> Value -> Eff es Value
index pos (Map _ m) ix = readMap pos m ix
index pos (Array _ v) ix = readArray pos v ix
index pos nonStructure ix = throwError (invalidIndex pos nonStructure ix)

evalAccess :: Eval :>> es => Position -> Name -> Expr -> Eff es Value
evalAccess pos field x = do
  val <- eval x
  access pos field val

access :: Eval :>> es => Position -> Name -> Value -> Eff es Value
access pos field (Map _ m) = readMap pos m (String field)
access pos _ nonMap = throwError (nonMapAccess pos nonMap)

evalCall :: Eval :>> es => Position -> Expr -> [Expr] -> Eff es Value
evalCall pos f args =
  eval f >>= \case
    Closure _ env params body -> do
      when (length params /= length args) do
        throwError (invalidArity pos (length params) (length args))
      vals <- traverse (liftIO . newIORef <=< eval) args
      let args' = Map.fromList (zip params vals)
      stack <- get @Stack
      put [args', env]
      ret <- tryError @(Value, Position) (local (pos :) (eval body))
      put stack
      pure (either (fst . snd) id ret)
    PrimOp op -> evalPrimOp pos op args
    nonFunction -> throwError (calledNonFunction pos nonFunction)

evalPrimOp :: Eval :>> es => Position -> PrimOp -> [Expr] -> Eff es Value
evalPrimOp pos op args = case op of
  Print -> do
    arg <- case args of
      [arg] -> eval arg
      _ -> throwError (invalidArity pos 1 (length args))
    case arg of
      Unit -> liftIO (print ())
      PrimOp _ -> liftIO (putStrLn "<primop>")
      Int x -> liftIO (print x)
      Float x -> liftIO (print x)
      Char c -> liftIO (putChar c)
      Bool b -> liftIO (putStrLn (bool "false" "true" b))
      String s -> liftIO (T.putStrLn s)
      Array _ _ -> liftIO (putStrLn "<array>")
      Map _ _ -> liftIO (putStrLn "<map>")
      Closure _ _ _ _ -> liftIO (putStrLn "<closure>")
    pure Unit
  ReadLine -> do
    unless (null args) do
      throwError (invalidArity pos 0 (length args))
    String <$> liftIO T.getLine
  Length -> do
    arg <- case args of
      [arg] -> eval arg
      _ -> throwError (invalidArity pos 1 (length args))
    case arg of
      Array _ v -> pure (Int (fromIntegral (MVec.length v)))
      _ -> throwError (invalidLength pos arg)

evalUnOp :: Eval :>> es => Position -> UnOp -> Expr -> Eff es Value
evalUnOp pos op x = do
  val <- eval x
  case op of
    Negate -> case val of
      Int y -> pure (Int (-y))
      Float y -> pure (Float (-y))
      _ -> throwError (nonNumberNegation pos val)
    Not -> case val of
      Bool b -> pure (Bool (not b))
      _ -> throwError (nonBoolNot pos val)
    BitNot -> case val of
      Int y -> pure (Int (complement y))
      _ -> throwError (nonIntBitNot pos val)

evalBinOp :: Eval :>> es => Position -> BinOp -> Expr -> Expr -> Eff es Value
evalBinOp pos op x y = do
  x' <- eval x
  y' <- eval y
  binOp pos op x' y'

binOp :: forall es. Eval :>> es => Position -> BinOp -> Value -> Value -> Eff es Value
binOp pos op l r = case op of
  Plus -> arithOp (+)
  Minus -> arithOp (-)
  Times -> arithOp (*)
  Divide -> arithOp divide
  Mod -> intOp mod
  Equal -> eq
  NotEqual -> neq
  LessThan -> cmpOp (<)
  LessThanEqual -> cmpOp (<=)
  GreaterThanEqual -> cmpOp (>=)
  GreaterThan -> cmpOp (>)
  And -> boolOp (&&)
  Or -> boolOp (||)
  BitAnd -> intOp (.&.)
  BitOr -> intOp (.|.)
  BitXor -> intOp xor
  BitShiftLeft -> shiftOp shiftL
  BitShiftRight -> shiftOp shiftR
  where
    arithOp :: (forall a. Arith a => a -> a -> a) -> Eff es Value
    arithOp f = case (l, r) of
      (Int x, Int y) -> pure (Int (f x y))
      (Int x, Float y) -> pure (Float (f (fromIntegral x) y))
      (Float x, Int y) -> pure (Float (f x (fromIntegral y)))
      (Float x, Float y) -> pure (Float (f x y))
      _ -> throwError (invalidBinOp pos op l r)
    eq = case (l, r) of
      (Unit, Unit) -> pure (Bool True)
      (PrimOp o, PrimOp o') -> pure (Bool (o == o'))
      (Int x, Int y) -> pure (Bool (x == y))
      (Float x, Float y) -> pure (Bool (x == y))
      (Char c, Char c') -> pure (Bool (c == c'))
      (Bool b, Bool b') -> pure (Bool (b == b'))
      (String s, String s') -> pure (Bool (s == s'))
      (Array u _, Array u' _) -> pure (Bool (u == u'))
      (Map u _, Map u' _) -> pure (Bool (u == u'))
      (Closure u _ _ _, Closure u' _ _ _) -> pure (Bool (u == u'))
      _ -> throwError (invalidBinOp pos op l r)
    neq = eq <&> \case Bool b -> Bool (not b); x -> x
    cmpOp :: (forall a. Ord a => a -> a -> Bool) -> Eff es Value
    cmpOp f = case (l, r) of
      (Unit, Unit) -> pure (Bool (f () ()))
      (PrimOp o, PrimOp o') -> pure (Bool (f o o'))
      (Int x, Int y) -> pure (Bool (f x y))
      (Float x, Float y) -> pure (Bool (f x y))
      (Char c, Char c') -> pure (Bool (f c c'))
      (Bool b, Bool b') -> pure (Bool (f b b'))
      (String s, String s') -> pure (Bool (f s s'))
      (Array u _, Array u' _) -> pure (Bool (f u u'))
      (Map u _, Map u' _) -> pure (Bool (f u u'))
      (Closure u _ _ _, Closure u' _ _ _) -> pure (Bool (f u u'))
      _ -> throwError (invalidBinOp pos op l r)
    intOp f = case (l, r) of
      (Int x, Int y) -> pure (Int (f x y))
      _ -> throwError (invalidBinOp pos op l r)
    boolOp f = case (l, r) of
      (Bool x, Bool y) -> pure (Bool (f x y))
      _ -> throwError (invalidBinOp pos op l r)
    shiftOp f = case (l, r) of
      (Int x, Int y)
        | y >= 0 -> pure (Int (f x (fromIntegral y)))
        | otherwise -> throwError (negativeShift pos)
      _ -> throwError (invalidBinOp pos op l r)

evalBlock :: Eval :>> es => Position -> [Statement] -> Maybe Expr -> Eff es Value
evalBlock _ stmts x = do
  pushFrame Map.empty
  traverse_ exec stmts
  val <- maybe (pure Unit) eval x
  dropFrame
  pure val

evalWhile :: Eval :>> es => Position -> Expr -> Expr -> Eff es Value
evalWhile pos cond body = go
  where
    go =
      eval cond >>= \case
        Bool False -> pure Unit
        Bool True -> eval body *> go
        nonBool -> throwError (nonBoolCondition pos nonBool)

evalIf :: Eval :>> es => Position -> Expr -> Expr -> Maybe Expr -> Eff es Value
evalIf pos cond x y =
  eval cond >>= \case
    Bool True -> eval x
    Bool False
      | Just y' <- y -> eval y'
      | otherwise -> pure Unit
    nonBool -> throwError (nonBoolCondition pos nonBool)

evalReturn :: Eval :>> es => Position -> Maybe Expr -> Eff es Value
evalReturn pos = \case
  Nothing -> throwError (Unit, pos)
  Just x -> do
    val <- eval x
    throwError (val, pos)

evalFunction :: Eval :>> es => Position -> [Name] -> Expr -> Eff es Value
evalFunction _ params body = do
  env <- gets @Stack (foldl' (flip Map.union) Map.empty)
  u <- liftIO newUnique
  pure (Closure u env params body)

readMap :: [IOE, RuntimeErrors] :>> es => Position -> CuckooHashTable Value Value -> Value -> Eff es Value
readMap pos m ix =
  liftIO (HT.lookup m ix) >>= \case
    Nothing -> throwError (keyNotInMap pos ix)
    Just val -> pure val

modifyMap :: [IOE, RuntimeErrors] :>> es => Position -> CuckooHashTable Value Value -> Value -> (Value -> Eff es Value) -> Eff es ()
modifyMap pos m ix f = do
  val <- readMap pos m ix
  val' <- f val
  liftIO (HT.insert m ix val')

readArray :: [Prim, RuntimeErrors] :>> es => Position -> IOVector Value -> Value -> Eff es Value
readArray pos v (Int ix)
  | fromIntegral ix >= MVec.length v = throwError (arrayOutOfBounds pos)
  | otherwise = MVec.read v (fromIntegral ix)
readArray pos _ ix = throwError (nonIntIndex pos ix)

modifyArray :: [Prim, RuntimeErrors] :>> es => Position -> IOVector Value -> Value -> (Value -> Eff es Value) -> Eff es ()
modifyArray pos v (Int ix) f
  | fromIntegral ix >= MVec.length v = throwError (arrayOutOfBounds pos)
  | otherwise = MVec.modifyM v f (fromIntegral ix)
modifyArray pos _ ix _ = throwError (nonIntIndex pos ix)

getVar :: State Stack :> es => Name -> Eff es (Maybe (IORef Value))
getVar name = gets @Stack (foldr ((<|>) . Map.lookup name) Nothing)

pushFrame :: State Stack :> es => Frame -> Eff es ()
pushFrame = modify . (:)

dropFrame :: State Stack :> es => Eff es ()
dropFrame = modify @Stack (drop 1)

makePrimOps :: IOE :> es => Eff es Frame
makePrimOps =
  traverse (liftIO . newIORef) $
    Map.fromList
      [ ("puts", PrimOp Print)
      , ("input", PrimOp ReadLine)
      , ("len", PrimOp Length)
      ]
