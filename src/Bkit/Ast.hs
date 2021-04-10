module Bkit.Ast where

import Data.List.NonEmpty (NonEmpty)

type Id = String

data Stmt = IfStmt {cond :: Expr, thenStmt :: StmtList, elseStmt :: StmtList}
		  | ForStmt {countIdx:: String, from::Expr, to::Expr, step::Expr, loop::StmtList}
			| WhileStmt Expr StmtList
			| CallStmt String [Expr]
			| BreakStmt | ContStmt | ReturnStmt (Maybe Expr)
			deriving (Show)

data StmtList = StmtList [VarDecl] [Stmt] deriving (Show)

data Expr = IdExpr String | CallExpr String [Expr] | LiteralExpr Literal
			| ArrayCell {arr:: Expr, idx :: NonEmpty Expr}
			| BinaryOp {op:: String, lhs:: Expr, rhs :: Expr}
			| UnaryOp {op:: String, body :: Expr }
			deriving (Show)

data Literal = BoolLiteral Bool | IntLiteral Integer | FloatLiteral Double
		| StringLiteral String | ArrayLiteral [Literal]
			deriving (Show)

data VarDecl = VarDecl { name :: Id, dim :: [Integer], initVal :: Literal }
	deriving (Show)

data FuncDecl = FuncDecl Id [String] StmtList
	deriving (Show)

data Program = Program [VarDecl] [FuncDecl]
