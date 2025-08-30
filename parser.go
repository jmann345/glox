package main

import (
	"fmt"
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

// NOTE:
// I am not in love with the current implementation as described in the book
// Namely, my issue is how we attempt to parse each expression
// in the chain of precendence without a dispatch function
// For example, we should only be parsing an if expr if we see an IF token
// And that could easily be handled by a function that dispatches to the
// relevant grammar rule based on the peeked token
// UPDATE: I think what I don't like is tree-walk interpreters in general :)
// Luckily, with the Clox we won't have to do that anymore!

type Parser struct {
	tokens  []Token
	current int
}

func (p *Parser) IsAtEnd() bool {
	return p.peekToken().typ == EOF
}

// program ::= declaration* EOF
func (p *Parser) Parse() (Stmt, error) {
	return p.parseDeclaration()
}

func (p *Parser) synchronize() {
	p.current++
	for p.current < len(p.tokens) {
		prev := p.tokens[p.current-1]
		if prev.typ == SEMICOLON {
			return
		}

		if p.peekIsOneOf(
			CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN) {
			return
		}
		p.current++
	}
}

// declaration ::= funDecl | varDecl | statement
func (p *Parser) parseDeclaration() (Stmt, error) {
	var stmt Stmt
	var err error
	switch p.peekToken().typ {
	case VAR:
		stmt, err = p.parseVarDecl()
	case FUN:
		stmt, err = p.parseFunDecl()
	default:
		stmt, err = p.parseStatement()
	}

	if err != nil {
		p.synchronize()
		return nil, err
	}

	return stmt, nil
}

// varDecl ::= "var" IDENTIFIER ( "=" expression )? ";"
func (p *Parser) parseVarDecl() (Stmt, error) {
	err := p.consumeToken(VAR, "Expect identifier 'var'.")
	if err != nil {
		return nil, err
	}

	name := p.peekToken()
	if err = p.consumeToken(IDENTIFIER, "Invalid Identifier."); err != nil {
		return nil, err
	}

	// TODO: Create a separate keyword for undefined
	// e.g.:
	/*
		var x;
		print x;
		>>> "undefined"
	*/
	var initializer Expr = Literal{nil}
	if p.peekToken().typ == EQUAL {
		p.current++ // consume '='

		initializer, err = p.parseExpression()
		if err != nil {
			return nil, err
		}
	}

	err = p.consumeToken(SEMICOLON, "Expect ';' after variable declaration.")
	if err != nil {
		return nil, err
	}

	return VarDecl{name, initializer}, nil
}

// funDecl ::= "fun" IDENTIFIER "(" parameters? ")" block
func (p *Parser) parseFunDecl() (Stmt, error) {
	if err := p.consumeToken(FUN, "Expect identifier 'fun'."); err != nil {
		return nil, err
	}

	name := p.peekToken()
	if err := p.consumeToken(IDENTIFIER, "Expect function name."); err != nil {
		return nil, err
	}

	err := p.consumeToken(LEFT_PAREN, "Expect '(' after function name.")
	if err != nil {
		return nil, err
	}

	params, err := p.parseParameters()
	if err != nil {
		return nil, err
	}

	err = p.consumeToken(RIGHT_PAREN, "Expect ')' after parameters.")
	if err != nil {
		return nil, err
	}

	body, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	return FunDecl{name, params, body}, nil
}

func (p *Parser) parseParameters() ([]Token, error) {
	params := []Token{}

	for !p.peekIsOneOf(RIGHT_PAREN, EOF) {
		param, ok := p.consumeOneOf(IDENTIFIER)
		if !ok {
			return nil, ParseError{param, "Expect parameter name."}
		}

		params = append(params, param)
		if len(params) >= 255 {
			return nil, ParseError{
				p.peekToken(),
				"Can't have more than 255 parameters.",
			}
		}

		if p.peekToken().typ != RIGHT_PAREN {
			err := p.consumeToken(COMMA, "Expect ',' between parameters.")
			if err != nil {
				return nil, err
			}
		}
	}

	return params, nil
}

// statement ::= exprStmt | printStmt | ifStmt | block
func (p *Parser) parseStatement() (Stmt, error) {
	switch p.peekToken().typ {
	case PRINT:
		return p.parsePrintStmt()
	case IF:
		return p.parseIfStmt()
	case WHILE:
		return p.parseWhileStmt()
	case FOR:
		return p.parseForStmt()
	case RETURN:
		return p.parseReturnStmt()
	case LEFT_BRACE:
		return p.parseBlock()
	default:
		return p.parseExprStmt()
	}
}

// exprStmt ::= expression ";"
func (p *Parser) parseExprStmt() (Stmt, error) {
	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	err = p.consumeToken(SEMICOLON, "Expect ';' after expression.")
	if err != nil {
		return nil, err
	}

	return ExprStmt{expr}, nil
}


// block ::= "{" declaration* "}"
func (p *Parser) parseBlock() (Stmt, error) {
	err := p.consumeToken(LEFT_BRACE, "Expect '{' before block.")
	if err != nil {
		return nil, err
	}

	stmts := []Stmt{}

	for !p.peekIsOneOf(RIGHT_BRACE, EOF) {
		stmt, err := p.parseDeclaration()
		if err != nil {
			return nil, err
		}

		stmts = append(stmts, stmt)
	}

	err = p.consumeToken(RIGHT_BRACE, "Expect '}' after block.")
	if err != nil {
		return nil, err
	}

	return Block{stmts}, nil
}

// print ::= "print" value ";"
func (p *Parser) parsePrintStmt() (Stmt, error) {
	if err := p.consumeToken(PRINT, "Expect 'print' statement."); err != nil {
		return nil, err
	}

	value, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if err = p.consumeToken(SEMICOLON, "Expect ';' after value."); err != nil {
		return nil, err
	}

	return PrintStmt{value}, nil
}

// if ::= "if" expression block ( "else" ( block | ifStmt ) )?
// NOTE: My ifStmt grammar is different from original lox implementation
func (p *Parser) parseIfStmt() (Stmt, error) {
	tok := p.peekToken()

	if err := p.consumeToken(IF, "Expect 'if' statement."); err != nil {
		return nil, err
	}

	cond, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	thenStmt, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	var elseStmt Stmt = nil
	if p.peekToken().typ == ELSE {
		p.current++ // consume "else"

		if p.peekToken().typ == IF {
			elseStmt, err = p.parseIfStmt()
		} else {
			elseStmt, err = p.parseBlock()
		}

		if err != nil {
			return nil, err
		}
	}

	return IfStmt{tok, cond, thenStmt, elseStmt}, nil
}

// while ::= "while" expression block
func (p *Parser) parseWhileStmt() (Stmt, error) {
	tok := p.peekToken()

	err := p.consumeToken(WHILE, "Expect 'while' statement.")
	if err != nil {
		return nil, err
	}

	cond, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	block, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	return WhileStmt{tok, cond, block}, nil
}

// for ::= "for" ( varDecl | exprStmt | ";" )
//
//	             expression? ";"
//	             expression?
//				 block
func (p *Parser) parseForStmt() (Stmt, error) {
	tok := p.peekToken()

	err := p.consumeToken(FOR, "Expect 'for' statement.")
	if err != nil {
		return nil, err
	}

	var initializer Stmt
	switch tok := p.peekToken(); tok.typ {
	case SEMICOLON:
		p.current++ // consume ';'
		initializer = NoOpStmt{}
	case VAR:
		initializer, err = p.parseVarDecl()
	default:
		initializer, err = p.parseExprStmt()
	}
	if err != nil {
		return nil, err
	}

	var condition Expr = NoOpExpr{}
	if p.peekToken().typ != SEMICOLON {
		condition, err = p.parseExpression()
		if err != nil {
			return nil, err
		}
	}

	p.consumeToken(SEMICOLON, "Expect ';' after 'for' condition.")

	var increment Expr = NoOpExpr{}
	if p.peekToken().typ != LEFT_BRACE {
		increment, err = p.parseExpression()
		if err != nil {
			return nil, err
		}
	}

	body, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	return ForStmt{tok, initializer, condition, increment, body}, nil
}

// expression ::= assignment
func (p *Parser) parseExpression() (Expr, error) {
	return p.parseAssignment()
}

// assignment ::= IDENTIFIER '=' assignment | comma
func (p *Parser) parseAssignment() (Expr, error) {
	expr, err := p.parseComma()
	if err != nil {
		return nil, err
	}

	if tok := p.peekToken(); tok.typ == EQUAL {
		p.current++ // consume '='

		value, err := p.parseAssignment()
		if err != nil {
			return nil, err
		}

		if expr, ok := expr.(*Variable); ok {
			return Assign{expr.name, value}, nil
		}

		return nil, ParseError{tok, "Invalid assignment target."}
	}

	return expr, nil
}

// CHALLENGE 1: Add comma (easy)
// comma ::= ifExpr ( , ifExpr )*
func (p *Parser) parseComma() (Expr, error) {
	if tok := p.peekToken(); tok.typ == COMMA {
		p.current++ // consume ','
		_, _ = p.parseIfExpr()

		err := ParseError{tok, "Missing left-hand operand for ','"}
		return nil, err
	}

	expr, err := p.parseIfExpr()
	if err != nil {
		return nil, err
	}

	for op := p.peekToken(); op.typ == COMMA; op = p.peekToken() {
		p.current++

		rhs, err := p.parseIfExpr()
		if err != nil {
			return nil, err
		}

		expr = Binary{expr, op, rhs}
	}

	return expr, nil
}

// CHALLENGE 2: Add ternary operator
// (I opted to do add the rust-style if expr instead)
// ifExpr ::= logicalOr | 'if' logicalOr { expr } 'else' ( { expr } | ifExpr )
func (p *Parser) parseIfExpr() (Expr, error) {
	if tok := p.peekToken(); tok.typ == IF {
		p.current++

		cond, err := p.parseLogicalOr()
		if err != nil {
			return nil, err
		}

		err = p.consumeToken(LEFT_BRACE, "Expect '{' after condition")
		if err != nil {
			return nil, err
		}

		thenExpr, err := p.parseExpression()
		if err != nil {
			return nil, err
		}

		err = p.consumeToken(RIGHT_BRACE, "Expect '}' after if-block.")
		if err != nil {
			return nil, err
		}

		err = p.consumeToken(ELSE, "Expect 'else' after 'if' expression.")
		if err != nil {
			return nil, err
		}

		var elseExpr Expr
		if p.peekToken().typ == IF {
			elseExpr, err = p.parseIfExpr()
			if err != nil {
				return nil, err
			}
		} else {
			err = p.consumeToken(LEFT_BRACE, "Expect '{' after 'else'.")
			if err != nil {
				return nil, err
			}

			elseExpr, err = p.parseExpression()
			if err != nil {
				return nil, err
			}

			err = p.consumeToken(RIGHT_BRACE, "Expect '}' after else-block.")
			if err != nil {
				return nil, err
			}
		}

		return IfExpr{tok, cond, thenExpr, elseExpr}, nil
	}

	return p.parseLogicalOr()
}

// logicalOr ::= logicalAnd ( "or" logicalAnd )*
func (p *Parser) parseLogicalOr() (Expr, error) {
	if tok, ok := p.consumeOneOf(OR); ok {
		_, _ = p.parseLogicalAnd()

		err := ParseError{tok, "Missing left-hand operand for 'or'"}
		return nil, err
	}

	expr, err := p.parseLogicalAnd()
	if err != nil {
		return nil, err
	}

	for p.peekToken().typ == OR {
		p.current++ // consume "or"
		op := p.tokens[p.current-1]

		rhs, err := p.parseLogicalAnd()
		if err != nil {
			return nil, err
		}

		expr = Binary{expr, op, rhs}
	}

	return expr, nil
}

// logic_or ::= equality ( "and" equality )*
func (p *Parser) parseLogicalAnd() (Expr, error) {
	if tok, ok := p.consumeOneOf(AND); ok {
		_, _ = p.parseEquality()

		err := ParseError{tok, "Missing left-hand operand for 'and'"}
		return nil, err
	}

	expr, err := p.parseEquality()
	if err != nil {
		return nil, err
	}

	for p.peekToken().typ == AND {
		p.current++ // consume "or"
		op := p.tokens[p.current-1]

		rhs, err := p.parseEquality()
		if err != nil {
			return nil, err
		}

		expr = &Binary{expr, op, rhs}
	}

	return expr, nil
}

// equality ::= comparison ( ('!=' | '==') comparison )*
func (p *Parser) parseEquality() (Expr, error) {
	if tok, ok := p.consumeOneOf(BANG_EQUAL, EQUAL_EQUAL); ok {
		_, _ = p.parseComparison()

		err := ParseError{tok,
			"Missing left-hand operand for '" + tok.lexeme + "'"}
		return nil, err
	}

	expr, err := p.parseComparison()
	if err != nil {
		return nil, err
	}

	// We loop here to support syntax like:
	// a < b < c (methinks)
	// NOTE: 'a < b < c' is currently unsupported tho :(
	for p.peekIsOneOf(BANG_EQUAL, EQUAL_EQUAL) {
		p.current++
		op := p.tokens[p.current-1]

		rhs, err := p.parseComparison()
		if err != nil {
			return nil, err
		}

		expr = &Binary{expr, op, rhs}
	}

	return expr, nil
}

// comparison ::= term ( ( '>' | '>=' | '<' | '<=' ) term )*
func (p *Parser) parseComparison() (Expr, error) {
	// if (tok := self.peekToken()) in (
	//     GREATER, GREATER_EQUAL, LESS, LESS_EQUAL
	// ): ...
	if tok, ok := p.consumeOneOf(
		GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
	); ok {
		_, _ = p.parseTerm()

		err := ParseError{tok,
			"Missing left-hand operand for '" + tok.lexeme + "'"}
		return nil, err
	}

	expr, err := p.parseTerm()
	if err != nil {
		return nil, err
	}

	// TODO: Allow for chain comparisons
	// e.g. we want to desugar 1 < 2 < 3  into (1 < 2) and (2 < 3)
	for p.peekIsOneOf(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) {
		p.current++

		op := p.tokens[p.current-1]
		rhs, err := p.parseTerm()
		if err != nil {
			return nil, err
		}

		expr = &Binary{expr, op, rhs}
	}

	return expr, nil
}

// term ::= factor ( ( '-' | '+' ) factor )*
func (p *Parser) parseTerm() (Expr, error) {
	if tok := p.peekToken(); tok.typ == PLUS {
		p.current++
		_, _ = p.parseFactor()

		err := ParseError{tok, "Missing left-hand operand for '+'"}
		return nil, err
	}

	expr, err := p.parseFactor()
	if err != nil {
		return nil, err
	}

	for p.peekIsOneOf(MINUS, PLUS) {
		p.current++

		op := p.tokens[p.current-1]
		rhs, err := p.parseFactor()
		if err != nil {
			return nil, err
		}

		expr = &Binary{expr, op, rhs}
	}

	return expr, nil
}

// factor ::= unary ( ( '*' | '/' ) unary )*
func (p *Parser) parseFactor() (Expr, error) {
	if tok, ok := p.consumeOneOf(STAR, SLASH); ok {
		_, _ = p.parseUnary()

		err := ParseError{tok,
			"Missing left-hand operand for '" + tok.lexeme + "'"}
		return nil, err
	}

	expr, err := p.parseUnary()
	if err != nil {
		return nil, err
	}

	for p.peekIsOneOf(SLASH, STAR) {
		p.current++

		op := p.tokens[p.current-1]
		rhs, err := p.parseUnary()
		if err != nil {
			return nil, err
		}

		expr = &Binary{expr, op, rhs}
	}

	return expr, nil
}

// unary ::= ( ( 'not' | '-' | '--' | '++' ) unary ) | postfix
func (p *Parser) parseUnary() (Expr, error) {
	if op, ok := p.consumeOneOf(NOT, MINUS, MINUS_MINUS, PLUS_PLUS); ok {
		rhs, err := p.parseUnary()
		if err != nil {
			return nil, err
		}

		return &Unary{op, rhs}, nil
	}

	return p.parsePostfix()
}

// postfix ::= call ( '--' | '++' )? )
func (p *Parser) parsePostfix() (Expr, error) {
	expr, err := p.parseCall()
	if err != nil {
		return nil, err
	}

	if op, ok := p.consumeOneOf(MINUS_MINUS, PLUS_PLUS); ok {
		return &Postfix{expr, op}, nil
	}

	return expr, nil
}

// call ::= primary ( "(" arguments? ")" )*
func (p *Parser) parseCall() (Expr, error) {
	expr, err := p.parsePrimary()
	if err != nil {
		return nil, err
	}

	if paren, ok := p.consumeOneOf(LEFT_PAREN); ok {
		args, err := p.parseArguments()
		if err != nil {
			return nil, err
		}

		err = p.consumeToken(RIGHT_PAREN, "Expect ')' after arguments.")
		if err != nil {
			return nil, err
		}

		return &CallExpr{expr, paren, args}, nil
	}

	return expr, nil
}

// arguments ::= expression ( "," expression )*
func (p *Parser) parseArguments() ([]Expr, error) {
	args := []Expr{}

	for !p.peekIsOneOf(RIGHT_PAREN, EOF) {
		// Can't use parseExpression() bc parseComma messes up the parsing
		// So we start at the next precedence level below parseComma
		// Also, parsing assignments is bad too here*. So we kill two birds with one stone!
		// * Unless we want to support keyword arguments, which we don't yet!

		arg, err := p.parseIfExpr()
		if err != nil {
			return nil, err
		}

		args = append(args, arg)

		if len(args) >= 255 {
			return nil, ParseError{
				p.peekToken(),
				"Can't have more than 255 arguments.",
			}
		}

		if p.peekToken().typ != RIGHT_PAREN {
			err = p.consumeToken(COMMA,
				"Expect ',' between function arguments.")
			if err != nil {
				return nil, err
			}
		}
	}

	return args, nil
}

/*
 * primary ::= NUMBER | STRING
 * 			 | 'true' | 'false' | 'nil'
 * 			 | ( expression )
 * 			 | IDENTIFIER
 */
func (p *Parser) parsePrimary() (Expr, error) {
	switch tok := p.peekAndConsume(); tok.typ {
	case NUMBER:
		return &Literal{tok.literal}, nil
	case STRING:
		return &Literal{tok.literal}, nil
	case TRUE:
		return &Literal{true}, nil
	case FALSE:
		return &Literal{false}, nil
	case NIL:
		return &Literal{nil}, nil
	case LEFT_PAREN:
		expr, err := p.parseExpression()
		if err != nil {
			return nil, err
		}

		err = p.consumeToken(RIGHT_PAREN, "Expect ')' after expression.")
		if err != nil {
			return nil, err
		}

		return &Grouping{expr}, nil
	case IDENTIFIER:
		return &Variable{tok}, nil
	default:
		return nil, ParseError{tok, "Expect expression."}
	}
}

func (p *Parser) peekToken() Token {
	p.current = min(p.current, len(p.tokens)-1) // avoid passing EOF
	return p.tokens[p.current]
}

func (p *Parser) peekIsOneOf(types ...TokenType) bool {
	return slices.Contains(types, p.peekToken().typ)
}

func (p *Parser) peekAndConsume() Token {
	tok := p.tokens[p.current]
	p.current++

	return tok
}

func (p *Parser) consumeToken(typ TokenType, message string) error {
	tok := p.peekToken()
	if tok.typ == typ {
		p.current++
		return nil
	}
	return ParseError{tok, message}
}

func (p *Parser) consumeOneOf(types ...TokenType) (Token, bool) {
	tok := p.peekToken()
	if slices.Contains(types, tok.typ) {
		p.current++
		return tok, true
	}

	return tok, false
}
