package main

import (
	"fmt"
	"slices"
)

type ParseError struct {
	tok Token
	msg string
}

func (e *ParseError) Error() string {
	if e.tok.typ == EOF {
		return fmt.Sprintf("%d at end\n%s", e.tok.line, e.msg)
	}
	return fmt.Sprintf("%d at '%s'\n%s", e.tok.line, e.tok.lexeme, e.msg)
}

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


// declaration ::= varDecl | statement
func (p *Parser) parseDeclaration() (Stmt, error) {
	var stmt Stmt
	var err error
	if p.peekToken().typ == VAR {
		p.current++
		stmt, err = p.parseVarDecl()
	} else {
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
	name := p.peekToken()
	err := p.consumeToken(IDENTIFIER, "Invalid Identifier.")
	if err != nil {
		return nil, err
	}

	// TODO: Create a separate keyword for undefined
	// e.g.:
	/*
		var x;
		print x;
		>>> "undefined"
	*/
	var initializer Expr = &Literal{nil}
	if p.peekToken().typ == EQUAL {
		p.current++

		initializer, err = p.parseExpression()
		if err != nil {
			return nil, err
		}
	}

	err = p.consumeToken(SEMICOLON, "Expect ';' after variable declaration.")
	if err != nil {
		return nil, err
	}

	return &VarDecl{name, initializer}, nil
}

// statement ::= exprStmt | printStmt
func (p *Parser) parseStatement() (Stmt, error) {
	if p.peekToken().typ == PRINT {
		p.current++ // consume PRINT token
		return p.parsePrintStmt()
	}

	return p.parseExprStmt()
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

	return &ExprStmt{expr}, nil
}

// print ::= "print" value ";"
func (p *Parser) parsePrintStmt() (Stmt, error) {
	value, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	err = p.consumeToken(SEMICOLON, "Expect ';' after value.")
	if err != nil {
		return nil, err
	}

	return &PrintStmt{value}, nil
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
			return &Assign{expr.name, value}, nil
		}

		return nil, &ParseError{
			tok, "Invalid assignment target.",
		}
	}

	return expr, nil
}

// CHALLENGE 1: Add comma (easy)
// comma ::= ifExpr ( , ifExpr )*
func (p *Parser) parseComma() (Expr, error) {
	if tok := p.peekToken(); tok.typ == COMMA {
		p.current++
		_, _ = p.parseIfExpr()

		err := &ParseError{tok, "Missing left-hand operand for ','"}
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

		expr = &Binary{expr, op, rhs}
	}

	return expr, nil
}

// CHALLENGE 2: Add ternary operator
// (I opted to do add the rust-style if expr instead)
// ifExpr ::= equality | 'if' equality { expr } 'else' ( { expr } | ifExpr )
func (p *Parser) parseIfExpr() (Expr, error) {
	if tok := p.peekToken(); tok.typ == IF {
		p.current++

		cond, err := p.parseEquality()
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

		return &IfExpr{tok, cond, thenExpr, elseExpr}, nil
	}

	return p.parseEquality()
}

// equality ::= comparison ( ('!=' | '==') comparison )*
func (p *Parser) parseEquality() (Expr, error) {
	if p.peekIsOneOf(BANG_EQUAL, EQUAL_EQUAL) {
		tok := p.peekToken()
		p.current++

		_, _ = p.parseComparison()

		err := &ParseError{tok,
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
	if p.peekIsOneOf(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) {
		tok := p.peekToken()
		p.current++

		_, _ = p.parseTerm()

		err := &ParseError{tok,
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

		err := &ParseError{tok, "Missing left-hand operand for '+'"}
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
	if p.peekIsOneOf(STAR, SLASH) {
		tok := p.peekToken()
		p.current++

		_, _ = p.parseUnary()

		err := &ParseError{tok,
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

// unary ::= ( ( 'not' | '-' ) unary ) | primary
func (p *Parser) parseUnary() (Expr, error) {
	if p.peekIsOneOf(NOT, MINUS) {
		op := p.peekToken()
		p.current++

		rhs, err := p.parseUnary()
		if err != nil {
			return nil, err
		}

		return &Unary{op, rhs}, nil
	}
	return p.parsePrimary()
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
		return nil, &ParseError{tok, "Expect expression."}
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
	return &ParseError{tok, message}
}
