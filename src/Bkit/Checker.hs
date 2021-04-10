{-# LANGUAGE OverloadedLists #-}

module Bkit.Checker (check) where

import qualified Data.Map.Strict as Map
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT(..), execStateT, modify)

import Bkit.Ast
import qualified Bkit.Context as Context

data Type = BoolType | IntType | FloatType | StringType | ArrayType {dim :: [Integer], elemType :: Type}
	deriving (Show, Eq)

data OpType = OpType { inType :: Type, outType :: Type }

data Kind = Function | Parameter | Variable | Identifier
	deriving (Show, Eq)

data StaticError = Undeclared Kind Id
				| Redeclared Kind Id
				| TypeMismatchInExpression Expr
  				| TypeCannotBeInferred Stmt
				| NoEntryPoint

-- type SymTab = [Map.Map Id Type]
type SymTab = Context.Context Id Type

type CheckerState = StateT SymTab (Either StaticError)

opSyms :: Map.Map String OpType
opSyms = Map.fromList $
	mapOp ["+", "-", "*", "\\", "%"] (OpType IntType IntType) ++
	mapOp ["+.", "-.", "*.", "\\."] (OpType FloatType FloatType) ++
	mapOp ["!", "&&", "||"] (OpType BoolType BoolType) ++
	mapOp ["==", "!=", "<", ">", "<=", ">="] (OpType IntType BoolType) ++
	mapOp ["=/=", "<.", ">.", "<=.", ">=."] (OpType FloatType BoolType)
	where
	mapOp ops opType = map (\x -> (x, opType)) ops

check :: Program -> Either StaticError SymTab
check = flip execStateT [] . checkProgram

checkProgram :: Program -> CheckerState ()
checkProgram (Program varDecls funcDecls) =
	foldM (const checkVarDecl) () varDecls

checkVarDecl :: VarDecl -> CheckerState ()
checkVarDecl (VarDecl varName varDim initVal) = StateT $ \s ->
	if Context.member varName s then Left (Redeclared Variable varName)
		else Right ((), Context.insert varName (literalType initVal) s)

checkExpr :: Expr -> CheckerState Type
checkExpr expr@(UnaryOp op body) =
	checkExpr body >>= checkOperandType op expr
checkExpr expr@(BinaryOp op lhs rhs) =
	checkExpr lhs >>= checkOperandType op expr >> checkExpr rhs >>= checkOperandType op expr
checkExpr (LiteralExpr literal) = return $ literalType literal

	--let opType = Map.lookup op symtab in
-- checkExpr (LiteralType literal) = return (literalType literal)

literalType :: Literal -> Type
literalType (BoolLiteral _) = BoolType
literalType (IntLiteral _) = IntType
literalType (FloatLiteral _) = FloatType

checkOperandType :: String -> Expr -> Type -> CheckerState Type
checkOperandType bodyType expr = lift $ if bodyType == inType opSym then Right (outType opSym) else Left (TypeMismatchInExpression expr)
	where
	opSym = Context.lookup opSyms op

isDeclared :: Id -> SymTab -> Bool
isDeclared _ [] = False
isDeclared name (x:xs) = Map.member name x
