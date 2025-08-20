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

// We'll use this later! I think we can use go's recover for this :)
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
func (p *Parser) Parse() (Expr, error) {
	return p.parseExpression()
}

// expr ::= comma
func (p *Parser) parseExpression() (Expr, error) {
	return p.parseComma()
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
// ifExpr ::= equality | if equality { expr } else ( { expr } | ifExpr )
func (p *Parser) parseIfExpr() (Expr, error) {
	if tok := p.peekToken(); tok.typ == IF {
		p.current++

		cond, err := p.parseEquality()
		if err != nil {
			return nil, err
		}

		ok := p.consumeToken(LEFT_BRACE)
		if !ok {
			err := &ParseError{
				p.peekToken(),
				"Expect '{' after condition",
			}
			return nil, err
		}

		thenExpr, err := p.parseExpression()
		if err != nil {
			return nil, err
		}

		ok = p.consumeToken(RIGHT_BRACE)
		if !ok {
			err := &ParseError{
				p.peekToken(),
				"Expect '}' after if-block.",
			}
			return nil, err
		}

		ok = p.consumeToken(ELSE)
		if !ok {
			err := &ParseError{
				p.peekToken(),
				"Expect 'else' after 'if' expression.",
			}
			return nil, err
		}

		var elseExpr Expr
		if p.peekToken().typ == IF {
			elseExpr, err = p.parseIfExpr()
			if err != nil {
				return nil, err
			}
		} else {
			ok = p.consumeToken(LEFT_BRACE)
			if !ok {
				err := &ParseError{
					p.peekToken(),
					"Expect '{' after 'else'.",
				}
				return nil, err
			}

			elseExpr, err = p.parseExpression()
			if err != nil {
				return nil, err
			}

			ok = p.consumeToken(RIGHT_BRACE)
			if !ok {
				err := &ParseError{
					p.peekToken(),
					"Expect '}' after else-block.",
				}
				return nil, err
			}
		}


		return &IfExpr{cond, thenExpr, elseExpr}, nil
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
 */
func (p *Parser) parsePrimary() (Expr, error) {
	tok := p.peekAndConsume()
	switch tok.typ {
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

		ok := p.consumeToken(RIGHT_PAREN)
		if !ok {
			err := &ParseError{
				p.peekToken(),
				"Expect ')' after expression.",
			}
			return nil, err
		}

		return &Grouping{expr}, nil
	}

	return nil, &ParseError{tok, "Expect expression."}
}

func (p *Parser) peekToken() Token {
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

func (p *Parser) consumeToken(typ TokenType) bool {
	if p.peekToken().typ == typ {
		p.current++
		return true
	}
	return false
}
