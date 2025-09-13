package main

import (
	"fmt"
)

type TokenType byte

const (
	// Single character tokens
	LEFT_PAREN TokenType = iota
	RIGHT_PAREN
	LEFT_BRACKET
	RIGHT_BRACKET
	LEFT_BRACE
	RIGHT_BRACE
	SEMICOLON
	COMMA
	DOT

	// Math operators
	MINUS
	PLUS
	SLASH
	STAR

	// Assignment
	EQUAL
	COLON_EQUAL // TODO: Add this for shorthand assignment

	// PREFIX/POSTFIX MATH OPS (ADDED)
	MINUS_MINUS
	PLUS_PLUS

	MINUS_EQUAL
	PLUS_EQUAL
	SLASH_EQUAL
	STAR_EQUAL

	// Comparison operators
	EQUAL_EQUAL
	BANG_EQUAL
	LESS
	LESS_EQUAL
	GREATER
	GREATER_EQUAL

	// Boolean keywords
	// NOTE: Lox officially uses BANG instead of NOT
	NOT
	AND
	OR
	// NIL keyword
	NIL
	// Boolean keywords
	TRUE
	FALSE
	// Control Flow keywords
	IF
	ELSE
	WHILE
	FOR
	BREAK
	CYCLE
	FUN
	RETURN
	// OOP Keywords
	SUPER
	THIS
	CLASS
	// Variable declaration keyword
	VAR
	// Misc. keyword(s)
	PRINT

	// Literals
	IDENTIFIER
	NUMBER
	STRING
	LIST

	EOF
)

func (t TokenType) String() string {
	switch t {
	case LEFT_PAREN:
		return "LEFT_PAREN"
	case RIGHT_PAREN:
		return "RIGHT_PAREN"
	case LEFT_BRACKET:
		return "LEFT_BRACKET"
	case RIGHT_BRACKET:
		return "RIGHT_BRACKET"
	case LEFT_BRACE:
		return "LEFT_BRACE"
	case RIGHT_BRACE:
		return "RIGHT_BRACE"
	case SEMICOLON:
		return "SEMICOLON"
	case COMMA:
		return "COMMA"
	case DOT:
		return "DOT"
	case MINUS:
		return "MINUS"
	case PLUS:
		return "PLUS"
	case SLASH:
		return "SLASH"
	case STAR:
		return "STAR"
	case MINUS_EQUAL:
		return "MINUS_EQUAL"
	case PLUS_EQUAL:
		return "PLUS_EQUAL"
	case SLASH_EQUAL:
		return "SLASH_EQUAL"
	case STAR_EQUAL:
		return "STAR_EQUAL"
	case EQUAL:
		return "EQUAL"
	case EQUAL_EQUAL:
		return "EQUAL_EQUAL"
	case BANG_EQUAL:
		return "BANG_EQUAL"
	case LESS:
		return "LESS"
	case LESS_EQUAL:
		return "LESS_EQUAL"
	case GREATER:
		return "GREATER"
	case GREATER_EQUAL:
		return "GREATER_EQUAL"
	case NOT:
		return "NOT"
	case AND:
		return "AND"
	case OR:
		return "OR"
	case NIL:
		return "NIL"
	case TRUE:
		return "TRUE"
	case FALSE:
		return "FALSE"
	case IF:
		return "IF"
	case ELSE:
		return "ELSE"
	case WHILE:
		return "WHILE"
	case FOR:
		return "FOR"
	case FUN:
		return "FUN"
	case RETURN:
		return "RETURN"
	case SUPER:
		return "SUPER"
	case THIS:
		return "THIS"
	case CLASS:
		return "CLASS"
	case VAR:
		return "VAR"
	case PRINT:
		return "PRINT"
	case IDENTIFIER:
		return "IDENTIFIER"
	case NUMBER:
		return "NUMBER"
	case STRING:
		return "STRING"
	case EOF:
		return "EOF"
	}

	panic(fmt.Sprintf("Invalid TokenType: %d", t))
}

type Token struct {
	typ     TokenType
	lexeme  string
	literal any
	line    int
}

func (t Token) String() string {
	return fmt.Sprintf("%s %s %v", t.typ, t.lexeme, t.literal)
}

func (t Token) UnderlyingOp() Token {
	switch t.typ {
	case MINUS_EQUAL:
		t.typ, t.lexeme = MINUS, "-"
	case PLUS_EQUAL:
		t.typ, t.lexeme = PLUS, "+"
	case SLASH_EQUAL:
		t.typ, t.lexeme = SLASH, "/"
	case STAR_EQUAL:
		t.typ, t.lexeme = STAR, "*"
	default:
		panic("Invalid token: " + t.String())
	}

	return t
}
