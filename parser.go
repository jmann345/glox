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
	tokens 	[]Token
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

// expr ::= equality
func (p *Parser) parseExpression() (Expr, error) {
	return p.parseEquality()
}

// equality ::= comparison ( (!= | ==) comparison )*
func (p *Parser) parseEquality() (Expr, error) {
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
// comparison ::= term ( ( > | >= | < |<=) term )*	
func (p *Parser) parseComparison() (Expr, error) {
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

// term ::= factor ( ( - | + ) factor )*
func (p *Parser) parseTerm() (Expr, error) {
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

// factor ::= unary ( ( / | * ) unary )*
func (p *Parser) parseFactor() (Expr, error) {
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

// unary ::= ( ! | - ) unary 
// 		   | primary
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
 * 			 | true | false | nil 
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
