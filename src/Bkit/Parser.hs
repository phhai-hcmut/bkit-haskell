module Bkit.Parser where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

import Text.Parsec.Expr as ExprP
import Data.List.NonEmpty (fromList)

import Bkit.Ast

bkitDef = emptyDef
	{ P.commentStart = "**"
	, P.commentEnd = "**"
	, P.nestedComments = False
	, P.identStart = lower
	, P.identLetter = alphaNum <|> char '_'
	, P.reservedNames = ["If", "Then"]
	}

lexer = P.makeTokenParser bkitDef
ident = P.identifier lexer
reserved = P.reserved lexer
integer = P.natural lexer
float = P.float lexer
stringLiteral = P.stringLiteral lexer
trueLiteral = reserved "True" >> return (BoolLiteral True)
falseLiteral = reserved "False" >> return (BoolLiteral False)
reservedOp = P.reservedOp lexer
brackets = P.brackets lexer
commaSep1 = P.commaSep1 lexer
commaSep = P.commaSep lexer
parens = P.parens lexer
symbol = P.symbol lexer
colon = P.colon lexer
semi = P.semi lexer
dot = P.dot lexer
comma = P.comma lexer

callExpr = CallExpr <$> ident <*> callArgs where
	callArgs = parens $ commaSep expr

arrIdx = fmap fromList $ many1 $ brackets expr
expr = ExprP.buildExpressionParser table exprTerm <?> "expression" where
	table = [--arrayCell
		  prefix <$> ["-", "-.", "!"]
		, binary <$> ["*", "*.", "\\", "\\."]
		, binary <$> ["+", "+.", "-", "-."]
		, binary' <$> ["==", "!=", "<", ">", "<=", ">=", "=/=", "<.", ">.", "<=.", ">=."]
		]
	binary  name = Infix (reservedOp name >> return (BinaryOp name)) ExprP.AssocLeft
	binary' name = Infix (reservedOp name >> return (BinaryOp name)) ExprP.AssocNone
	prefix  name = Prefix (reservedOp name >> return (UnaryOp name))
	-- arrayCell = Postfix (do {arrayIdx <- arrIdx; return (`ArrayCell` arrayIdx)})

exprTerm = parens expr <|> fmap LiteralExpr literal <|> try callExpr <|> fmap IdExpr ident

literal = trueLiteral <|> falseLiteral <|> fmap IntLiteral integer <|> fmap FloatLiteral float <|> fmap StringLiteral stringLiteral <|> arrayLiteral
arrayLiteral = fmap ArrayLiteral $ brackets $ commaSep1 literal

ifStmt = do
	reserved "If"
	cond <- expr
	reserved "Then"
	thenStmt <- stmtList
	elseStmt <- elseIfPart
	reserved "EndIf"
	dot
	return (IfStmt cond thenStmt elseStmt)
	where
	elseIfPart = elseIfStmt <|> elseClause <|> return (StmtList [] [])
	elseIfStmt = do
		reserved "ElseIf"
		cond <- expr
		reserved "Then"
		thenStmt <- stmtList
		elseStmt <- elseIfPart
		return (StmtList [] [IfStmt cond thenStmt elseStmt])
	elseClause = reserved "Else" >> stmtList

whileStmt = do
	reserved "While"
	cond <- expr
	reserved "Do"
	loop <- stmtList
	reserved "EndWhile"
	dot
	return (WhileStmt cond loop)

stmtList = StmtList <$> varDeclPart <*> many otherStmt

breakStmt = reserved "Break" >> semi >> return BreakStmt

contStmt = reserved "Continue" >> semi >> return ContStmt

otherStmt = ifStmt <|> whileStmt <|> breakStmt <|> contStmt <|> callStmt <|> returnStmt

returnStmt = do
	reserved "Return"
	returnExpr <- optionMaybe expr
	semi
	return (ReturnStmt returnExpr)

callStmt = do
	CallExpr name args <- callExpr
	semi
	return (CallStmt name args)

forStmt = do
	reserved "For"
	(idx, from, to, step) <- forCond
	reserved "Do"
	loop <- stmtList
	reserved "EndFor"; dot
	return (ForStmt idx from to step loop)
	where
	forCond = do
		idx <- ident
		symbol "="
		from <- expr
		comma
		to <- expr
		comma
		step <- expr
		return (idx, from, to, step)

varDeclPart = concat <$> many varDeclStmt
varDeclStmt = do
	reserved "Var"
	colon
	varDecls <- commaSep1 varDecl
	semi
	return varDecls
	where
		varDecl = do
			name <- ident
			dim <- many $ brackets integer
			symbol "="
			initVal <- literal
			return (VarDecl name dim initVal)

funcDecl = FuncDecl <$> funcName <*> funcParam <*> funcBody where
	funcName = reserved "Function" >> colon >> ident
	funcParam = option [] (reserved "Parameter" >> colon >> commaSep1 ident)
	funcBody = do
		reserved "Body"; colon
		body <- stmtList
		reserved "EndBody"; dot
		return body
