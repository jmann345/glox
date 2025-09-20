package main

import (
	"fmt"
	"os"
	"slices"
)

type ParseError struct {
	tok Token
	msg string
}

func (e ParseError) Error() string {
	if e.tok.typ == EOF {
		return fmt.Sprintf("%d at end\n%s", e.tok.line, e.msg)
	}
	return fmt.Sprintf("%d at '%s'\n%s", e.tok.line, e.tok.lexeme, e.msg)
}

type Parser struct {
	tokens   []Token
	current  int
	HadError bool
}

func NewParser(tokens []Token) *Parser {
	return &Parser{tokens, 0, false}
}

func (p *Parser) IsAtEnd() bool {
	return p.peekToken().typ == EOF
}

// program ::= declaration* EOF
func (p *Parser) Parse() Stmt {
	return p.parseDeclaration()
}

func (p *Parser) report(err ParseError) {
	p.HadError = true
	fmt.Fprintln(os.Stderr, "Parser:", err)
}

func (p *Parser) peekToken() Token {
	p.current = min(p.current, len(p.tokens)-1) // avoid passing EOF
	return p.tokens[p.current]
}

func (p *Parser) peekNextToken() Token {
	if p.current+1 >= len(p.tokens) {
		return p.peekToken()
	}

	return p.tokens[p.current+1]
}

func (p *Parser) peekIsOneOf(types ...TokenType) bool {
	return slices.Contains(types, p.peekToken().typ)
}

func (p *Parser) peekAndConsume() Token {
	tok := p.tokens[p.current]
	p.current++

	return tok
}

func (p *Parser) consumeToken(typ TokenType, message string) Token {
	if tok := p.peekToken(); tok.typ == typ {
		p.current++
		return tok
	} else {
		panic(ParseError{tok, message})
	}
}

func (p *Parser) tryConsume(typ TokenType) bool {
	if p.peekToken().typ == typ {
		p.current++
		return true
	}

	return false
}

func (p *Parser) consumeOneOf(types ...TokenType) (Token, bool) {
	tok := p.peekToken()
	if slices.Contains(types, tok.typ) {
		p.current++
		return tok, true
	}

	return tok, false
}

func (p *Parser) synchronize() {
	for p.current++; p.current < len(p.tokens); p.current++ {
		prev := p.tokens[p.current-1]
		if prev.typ == SEMICOLON {
			return
		}

		if p.peekIsOneOf(CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN) {
			return
		}
	}
}

// declaration ::= classDecl | funDecl | varDecl | statement
func (p *Parser) parseDeclaration() (stmt Stmt) {
	defer func() {
		if r := recover(); r != nil {
			p.HadError = true
			if err, ok := r.(ParseError); ok {
				p.report(err)
				p.synchronize()
				stmt = &NoOpStmt{}
			} else {
				panic(r) // real panic, let it crash
			}
		}
	}()

	switch p.peekToken().typ {
	case CLASS:
		stmt = p.parseClassDecl()
	case VAR:
		stmt = p.parseVarDecl()
	case FUN:
		// Check next token so we don't parse anonymous functions here
		if p.peekNextToken().typ != LEFT_PAREN {
			stmt = p.parseFunDecl("function")
			break // so we don't fallthrough to default
		}
		fallthrough
	default:
		stmt = p.parseStatement()
	}

	return
}

// classDecl ::= "class" IDENTIFIER "{" funDecl* "}"
func (p *Parser) parseClassDecl() Stmt {
	p.consumeToken(CLASS, "Expect 'class'.")

	name := p.consumeToken(IDENTIFIER, "Expect class name.")

	p.consumeToken(LEFT_BRACE, "Expect '{' before class body.")

	methods := []Stmt{}
	staticMethods := []Stmt{}
	for !p.peekIsOneOf(RIGHT_BRACE, EOF) {
		static := false
		if p.tryConsume(CLASS) {
			static = true
		}

		method := p.parseFunDecl("method")

		if static {
			staticMethods = append(staticMethods, method)
		} else {
			methods = append(methods, method)
		}
	}

	p.consumeToken(RIGHT_BRACE, "Expect '}' after class body.")

	return &ClassDecl{name, methods, staticMethods}
}

// varDecl ::= "var" IDENTIFIER ( "=" expression )? ";"
func (p *Parser) parseVarDecl() Stmt {
	p.consumeToken(VAR, "Expect identifier 'var'.")

	name := p.consumeToken(IDENTIFIER, "Invalid Identifier.")

	// TODO: Create a separate keyword for undefined
	// e.g.:
	/*
		var x;
		print x;
		>>> "undefined"
	*/
	var initializer Expr = &Literal{nil}
	if p.peekToken().typ == EQUAL {
		p.current++ // consume '='

		initializer = p.parseExpression()
	}

	p.consumeToken(SEMICOLON, "Expect ';' after variable declaration.")

	return &VarDecl{name, initializer}
}

// funDecl ::= "fun" IDENTIFIER "(" parameters? ")" block
func (p *Parser) parseFunDecl(funTypStr string) Stmt {
	if funTypStr == "function" {
		p.consumeToken(FUN, "Expect 'fun'.")
	}

	name := p.consumeToken(IDENTIFIER, "Expect "+funTypStr+" name.")

	p.consumeToken(LEFT_PAREN, "Expect '(' after "+funTypStr+" name.")
	params := p.parseParameters()
	p.consumeToken(RIGHT_PAREN, "Expect ')' after parameters.")

	body := p.parseBlock()

	return &FunDecl{name, params, body}
}

func (p *Parser) parseParameters() []Token {
	params := []Token{}

	for !p.peekIsOneOf(RIGHT_PAREN, EOF) {
		param := p.consumeToken(IDENTIFIER, "Expect parameter name.")
		params = append(params, param)
		if len(params) >= 255 {
			p.report(ParseError{p.peekToken(),
				"Can't have more than 255 parameters.",
			})
		}

		if p.peekToken().typ != RIGHT_PAREN {
			p.consumeToken(COMMA, "Expect ',' between parameters.")
		}
	}

	return params
}

// statement ::= exprStmt | printStmt | ifStmt | block
func (p *Parser) parseStatement() Stmt {
	switch p.peekToken().typ {
	case PRINT:
		return p.parsePrintStmt()
	case IF:
		return p.parseIfStmt()
	case WHILE:
		return p.parseWhileStmt()
	case FOR:
		return p.parseForStmt()
	case BREAK:
		return p.parseBreakStmt()
	case CYCLE:
		return p.parseCycleStmt()
	case RETURN:
		return p.parseReturnStmt()
	case LEFT_BRACE:
		return p.parseBlock()
	default:
		return p.parseExprStmt()
	}
}

// exprStmt ::= expression ";"
func (p *Parser) parseExprStmt() Stmt {
	expr := p.parseExpression()
	p.consumeToken(SEMICOLON, "Expect ';' after expression.")

	return &ExprStmt{expr}
}

// block ::= "{" declaration* "}"
func (p *Parser) parseBlock() Stmt {
	p.consumeToken(LEFT_BRACE, "Expect '{' before block.")

	stmts := []Stmt{}
	for !p.peekIsOneOf(RIGHT_BRACE, EOF) {
		stmts = append(stmts, p.parseDeclaration())
	}

	p.consumeToken(RIGHT_BRACE, "Expect '}' after block.")

	return &Block{stmts}
}

// print ::= "print" value ";"
func (p *Parser) parsePrintStmt() Stmt {
	p.consumeToken(PRINT, "Expect 'print' statement.")

	value := p.parseExpression()
	p.consumeToken(SEMICOLON, "Expect ';' after value.")

	return &PrintStmt{value}
}

// if ::= "if" expression block ( "else" ( block | ifStmt ) )?
// NOTE: My ifStmt grammar is different from original lox implementation
func (p *Parser) parseIfStmt() Stmt {
	tok := p.consumeToken(IF, "Expect 'if' statement.")

	cond := p.parseExpression()

	thenStmt := p.parseBlock()

	var elseStmt Stmt = nil
	if p.tryConsume(ELSE) {
		if p.peekToken().typ == IF {
			elseStmt = p.parseIfStmt()
		} else {
			elseStmt = p.parseBlock()
		}
	}

	return &IfStmt{tok, cond, thenStmt, elseStmt}
}

// while ::= "while" expression block
func (p *Parser) parseWhileStmt() Stmt {
	tok := p.peekToken()

	p.consumeToken(WHILE, "Expect 'while' statement.")

	cond := p.parseExpression()

	block := p.parseBlock()

	return &WhileStmt{tok, cond, block}
}

// for ::= "for" ( varDecl | exprStmt | ";" ) expression? ";" expression?
//				 block
func (p *Parser) parseForStmt() Stmt {
	tok := p.consumeToken(FOR, "Expect 'for' statement.")

	var initializer Stmt
	if p.tryConsume(SEMICOLON) {
		initializer = &NoOpStmt{}
	} else if p.peekToken().typ == VAR {
		initializer = p.parseVarDecl()
	} else {
		initializer = p.parseExprStmt()
	}

	var condition Expr = &NoOpExpr{}
	if p.peekToken().typ != SEMICOLON {
		condition = p.parseExpression()
	}
	p.consumeToken(SEMICOLON, "Expect ';' after 'for' condition.")

	var increment Expr = &NoOpExpr{}
	if p.peekToken().typ != LEFT_BRACE {
		increment = p.parseExpression()
	}

	body := p.parseBlock()

	return &ForStmt{tok, initializer, condition, increment, body}
}

// break ::= "break" ";"
func (p *Parser) parseBreakStmt() Stmt {
	tok := p.consumeToken(BREAK, "Expect break.")
	p.consumeToken(SEMICOLON, "Expect ';' after 'break'.")

	return &BreakStmt{tok}
}

// cycle ::= "cycle" ";"
func (p *Parser) parseCycleStmt() Stmt {
	tok := p.consumeToken(CYCLE, "Expect cycle.")
	p.consumeToken(SEMICOLON, "Expect ';' after 'cycle'.")

	return &CycleStmt{tok}
}

// return ::= "return" expression? ";"
func (p *Parser) parseReturnStmt() Stmt {
	tok := p.consumeToken(RETURN, "Expect return.")

	var value Expr = nil
	if p.peekToken().typ != SEMICOLON {
		value = p.parseExpression()
	}
	p.consumeToken(SEMICOLON, "Expect ';' after return value.")

	return &ReturnStmt{tok, value}
}

// expression ::= assignment
func (p *Parser) parseExpression() Expr {
	return p.parseComma()
}

// CHALLENGE 1: Add comma (easy)
// comma ::= assignment ( , assignment )*
func (p *Parser) parseComma() Expr {
	if tok, ok := p.consumeOneOf(COMMA); ok {
		_ = p.parseAssignment()
		p.report(ParseError{tok, "Missing left-hand operand for ','"})
		return &NoOpExpr{}
	}

	expr := p.parseAssignment()

	for op := p.peekToken(); op.typ == COMMA; op = p.peekToken() {
		p.current++
		rhs := p.parseAssignment()
		expr = &Binary{expr, op, rhs}
	}

	return expr
}

// assignment ::= ternary ( ( "=" | "-=" | "+=" | "/=" | "*=" ) assignment )?
func (p *Parser) parseAssignment() Expr {
	expr := p.parseTernary()

	if tok := p.peekToken(); tok.typ == EQUAL {
		p.current++ // consume '='

		value := p.parseAssignment()

		switch e := expr.(type) {
		case *Variable:
			return &Assign{e.name, value}
		case *Index:
			return &SetIndex{e.list, e.bracket, e.index, value}
		case *Get:
			return &Set{e.object, e.name, value}
		default:
			p.report(ParseError{tok, "Invalid assignment target."})
		}
	}

	if tok, ok := p.consumeOneOf(
		MINUS_EQUAL, PLUS_EQUAL, SLASH_EQUAL, STAR_EQUAL,
	); ok {
		op := tok.UnderlyingOp()
		rhs := p.parseAssignment()

		switch e := expr.(type) {
		case *Variable: // Desugar lhs .= rhs into lhs = lhs . rhs
			return &Assign{e.name, &Binary{e, op, rhs}}
		case *Index:
			return &AugSetIndex{e.list, e.bracket, e.index, op, rhs}
		case *Get:
			return &AugSet{e.object, e.name, op, rhs}
		default:
			p.report(ParseError{op, "Invalid assignment target."})
		}
	}

	return expr
}

// CHALLENGE 2: Add ternary operator
// ternary ::= logicalOr ("?" expression ":" ternary)?
func (p *Parser) parseTernary() Expr {
	if op, ok := p.consumeOneOf(QUESTION_MARK); ok {
		_ = p.parseLogicalOr()
		p.report(ParseError{op, "Missing left-hand operand for '?'"})
		return &NoOpExpr{}
	}

	expr := p.parseLogicalOr()

	if op, ok := p.consumeOneOf(QUESTION_MARK); ok {
		trueBranch := p.parseExpression()

		p.consumeToken(COLON, "Expect ':' in ternary expression.")

		falseBranch := p.parseTernary()

		return &Ternary{
			token:       op,
			condition:   expr,
			trueBranch:  trueBranch,
			falseBranch: falseBranch,
		}
	}

	return expr
}

// logicalOr ::= logicalAnd ( "or" logicalAnd )*
func (p *Parser) parseLogicalOr() Expr {
	if op, ok := p.consumeOneOf(OR); ok {
		_ = p.parseLogicalAnd()
		p.report(ParseError{op, "Missing left-hand operand for 'or'"})
	}

	expr := p.parseLogicalAnd()

	for p.peekToken().typ == OR {
		op := p.peekAndConsume()
		rhs := p.parseLogicalAnd()
		expr = &Binary{expr, op, rhs}
	}

	return expr
}

// logic_or ::= equality ( "and" equality )*
func (p *Parser) parseLogicalAnd() Expr {
	if op, ok := p.consumeOneOf(AND); ok {
		_ = p.parseEquality()
		p.report(ParseError{op, "Missing left-hand operand for 'and'"})
	}

	expr := p.parseEquality()

	for p.peekToken().typ == AND {
		op := p.peekAndConsume()
		rhs := p.parseEquality()
		expr = &Binary{expr, op, rhs}
	}

	return expr
}

// equality ::= comparison ( ('!=' | '==') comparison )*
func (p *Parser) parseEquality() Expr {
	if op, ok := p.consumeOneOf(BANG_EQUAL, EQUAL_EQUAL); ok {
		_ = p.parseComparison()
		p.report(ParseError{op,
			"Missing left-hand operand for '" + op.lexeme + "'"})
	}

	expr := p.parseComparison()
	// TODO: Support syntax for 'a == b != c'
	for p.peekIsOneOf(BANG_EQUAL, EQUAL_EQUAL) {
		op := p.peekAndConsume()
		rhs := p.parseComparison()
		expr = &Binary{expr, op, rhs}
	}

	return expr
}

// comparison ::= term ( ( '>' | '>=' | '<' | '<=' ) term )*
func (p *Parser) parseComparison() Expr {
	// The python syntax for this looks cool too:
	// if (op := self.peekToken()) in (
	//     GREATER, GREATER_EQUAL, LESS, LESS_EQUAL
	// ): ...
	if op, ok := p.consumeOneOf(
		GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
	); ok {
		_ = p.parseTerm()
		p.report(ParseError{op,
			"Missing left-hand operand for '" + op.lexeme + "'"})
	}

	expr := p.parseTerm()

	// TODO: Allow for chain comparisons
	// e.g. we want to desugar 1 < 2 < 3  into (1 < 2) and (2 < 3)
	for p.peekIsOneOf(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) {
		op := p.peekAndConsume()
		rhs := p.parseTerm()
		expr = &Binary{expr, op, rhs}
	}

	return expr
}

// term ::= factor ( ( '-' | '+' ) factor )*
func (p *Parser) parseTerm() Expr {
	if op, ok := p.consumeOneOf(PLUS); ok {
		_ = p.parseFactor()
		p.report(ParseError{op, "Missing left-hand operand for '+'"})
	}

	expr := p.parseFactor()
	for p.peekIsOneOf(MINUS, PLUS) {
		op := p.peekAndConsume()
		rhs := p.parseFactor()
		expr = &Binary{expr, op, rhs}
	}

	return expr
}

// factor ::= unary ( ( '*' | '/' ) unary )*
func (p *Parser) parseFactor() Expr {
	if op, ok := p.consumeOneOf(STAR, SLASH); ok {
		_ = p.parseUnary()
		p.report(ParseError{op,
			"Missing left-hand operand for '" + op.lexeme + "'"})
	}

	expr := p.parseUnary()
	for p.peekIsOneOf(SLASH, STAR) {
		op := p.peekAndConsume()
		rhs := p.parseUnary()
		expr = &Binary{expr, op, rhs}
	}

	return expr
}

// unary ::= ( ( 'not' | '-' | '--' | '++' ) unary ) | postfix
func (p *Parser) parseUnary() Expr {
	if op, ok := p.consumeOneOf(NOT, MINUS, MINUS_MINUS, PLUS_PLUS); ok {
		rhs := p.parseUnary()
		return &Unary{op, rhs}
	}

	return p.parsePostfix()
}

// postfix ::= suffix ( '--' | '++' )?
func (p *Parser) parsePostfix() Expr {
	expr := p.parseSuffix()
	if op, ok := p.consumeOneOf(MINUS_MINUS, PLUS_PLUS); ok {
		return &Postfix{expr, op}
	}

	return expr
}

/*
 * suffix ::= primary ( "(" arguments? ")"
 *					  | "[" assignment "]"
 *	                  | "." IDENTIFIER
 *				      )*
 */
func (p *Parser) parseSuffix() Expr {
	expr := p.parsePrimary()

	for p.peekIsOneOf(LEFT_PAREN, LEFT_BRACKET, DOT) {
		switch tok := p.peekAndConsume(); tok.typ {
		case LEFT_PAREN:
			args := p.parseArguments()
			p.consumeToken(RIGHT_PAREN, "Expect ')' after arguments.")
			expr = &CallExpr{expr, tok, args}
		case LEFT_BRACKET:
			index := p.parseAssignment()
			p.consumeToken(RIGHT_BRACKET, "Expect ']' after index.")
			expr = &Index{expr, tok, index}
		case DOT:
			name := p.consumeToken(IDENTIFIER, "Expect property name after '.'")
			expr = &Get{expr, name}
		}
	}

	return expr
}

// arguments ::= expression ( "," expression )*
func (p *Parser) parseArguments() []Expr {
	args := []Expr{}

	for !p.peekIsOneOf(RIGHT_PAREN, EOF) {
		arg := p.parseAssignment()
		args = append(args, arg)

		if len(args) >= 255 {
			p.report(ParseError{p.peekToken(),
				"Can't have more than 255 arguments.",
			})
		}

		if p.peekToken().typ != RIGHT_PAREN {
			p.consumeToken(COMMA, "Expect ',' between function arguments.")
		}
	}

	return args
}

/*
 * primary ::= NUMBER | STRING
 * 			 | 'true' | 'false' | 'nil'
 * 			 | ( expression )
 * 			 | IDENTIFIER
 * 			 | list
 * 			 | anonFunction
 */
func (p *Parser) parsePrimary() Expr {
	switch tok := p.peekAndConsume(); tok.typ {
	case NUMBER:
		return &Literal{tok.literal}
	case STRING:
		return &Literal{tok.literal}
	case TRUE:
		return &Literal{true}
	case FALSE:
		return &Literal{false}
	case NIL:
		return &Literal{nil}
	case LEFT_PAREN:
		expr := p.parseExpression()
		p.consumeToken(RIGHT_PAREN, "Expect ')' after expression.")

		return &Grouping{expr}
	case THIS:
		return &This{tok}
	case IDENTIFIER:
		return &Variable{tok}
	case LEFT_BRACKET:
		return p.parseList()
	case FUN:
		return p.parseAnonFunction()
	default:
		panic(ParseError{tok, "Expect expression."})
	}
}

// list ::= "[" ( assignment "," )* (assignment ","?)? "]"
func (p *Parser) parseList() Expr {
	lst := []Expr{}

	for !p.peekIsOneOf(RIGHT_BRACKET, EOF) {
		ele := p.parseAssignment()
		lst = append(lst, ele)

		if p.peekToken().typ != RIGHT_BRACKET {
			p.consumeToken(COMMA, "Expect ',' between list elements.")
		}
	}

	p.consumeToken(RIGHT_BRACKET, "Expect ']' after list.")

	return &List{lst}
}

// anonFunction ::= "fun" "(" parameters? ")" block
func (p *Parser) parseAnonFunction() Expr {
	tok := p.tokens[p.current-1] // Will always be a FUN token

	p.consumeToken(LEFT_PAREN, "Expect '(' after 'fun'.")
	params := p.parseParameters()
	p.consumeToken(RIGHT_PAREN, "Expect ')' after parameters.")

	body := p.parseBlock()

	return &FunExpr{tok, params, body}
}
