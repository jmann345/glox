package main

import (
	"slices"
	"strconv"
)

var keywords = map[string]TokenType{
	"and":    AND,
	"class":  CLASS,
	"else":   ELSE,
	"false":  FALSE,
	"for":    FOR,
	"fun":    FUN,
	"if":     IF,
	"nil":    NIL,
	"not":    NOT,
	"or":     OR,
	"print":  PRINT,
	"return": RETURN,
	"super":  SUPER,
	"this":   THIS,
	"true":   TRUE,
	"var":    VAR,
	"while":  WHILE,
}

type Tokenizer struct {
	source string
	tokens []Token

	start   int
	current int
	line    int
	// TODO: add column int (not in tutorial)
}

func (t *Tokenizer) Init(source string) {
	t.source = source
	t.tokens = make([]Token, 0)

	t.start = 0
	t.current = 0
	t.line = 1
}

func (t *Tokenizer) Tokenize() ([]Token, []error) {
	errs := make([]error, 0)
	srcLen := len(t.source)
	for t.current < srcLen {
		t.start = t.current
		err := t.scanToken()
		if err != nil {
			errs = append(errs, err)
		}
	}
	t.tokens = append(t.tokens, Token{EOF, "", nil, t.line})
	return t.tokens, errs
}

func (t *Tokenizer) scanToken() error {
	c := t.source[t.current]
	t.current++
	switch c {
	case ' ', '\r', '\t':
		; // pass
	case '\n':
		t.line++
	case '(':
		t.addToken(LEFT_PAREN, nil)
	case ')':
		t.addToken(RIGHT_PAREN, nil)
	case '{':
		t.addToken(LEFT_BRACE, nil)
	case '}':
		t.addToken(RIGHT_BRACE, nil)
	case ',':
		t.addToken(COMMA, nil)
	case '.':
		t.addToken(DOT, nil)
	case '-':
		t.addToken(MINUS, nil)
	case '+':
		t.addToken(PLUS, nil)
	case ';':
		t.addToken(SEMICOLON, nil)
	case '*':
		t.addToken(STAR, nil)
	case '/':
		t.addToken(SLASH, nil)
	case '!': // TODO: Add `not` keyword!
		if t.match('=') {
			t.addToken(BANG_EQUAL, nil)
		} else {
			return &LoxError{
				line:    t.line,
				where:   "",
				message: "Unexpected character.",
			}
		}
	case '=':
		if t.match('=') {
			t.addToken(EQUAL_EQUAL, nil)
		} else {
			t.addToken(EQUAL, nil)
		}
	case '<':
		if t.match('=') {
			t.addToken(LESS_EQUAL, nil)
		} else {
			t.addToken(LESS, nil)
		}
	case '>':
		if t.match('=') {
			t.addToken(GREATER_EQUAL, nil)
		} else {
			t.addToken(GREATER, nil)
		}
	case '"':
		err := t.scanString()
		if err != nil {
			return err
		}
	case '#':
		err := t.scanComment()
		if err != nil {
			return err
		}
	default:
		switch {
		case isDigit(c):
			err := t.scanNumber()
			if err != nil {
				return err
			}
		case isAlpha(c):
			t.scanIdentifierOrKeyword()
		default:
			return &LoxError{
				line:    t.line,
				where:   "",
				message: "Unexpected character.",
			}
		}
	}
	return nil
}

func (t *Tokenizer) addToken(typ TokenType, literal any) {
	text := t.source[t.start:t.current] // maybe t.current+1?
	t.tokens = append(
		t.tokens,
		Token{
			typ:     typ,
			lexeme:  text,
			literal: literal,
			line:    t.line,
		},
	)
}

func (t *Tokenizer) match(expected byte) bool {
	if t.current >= len(t.source) {
		return false
	}
	actual := t.source[t.current]
	if actual != expected {
		return false
	}
	t.current++
	return true
}

func (t *Tokenizer) peekAndConsume(expected string) bool {
	expectedLen := len(expected)
	if t.current+expectedLen > len(t.source) {
		return false
	}
	actual := t.source[t.current : t.current+expectedLen]
	if actual != expected {
		return false
	}
	t.current += expectedLen
	return true
}

func (t *Tokenizer) peekChar() byte {
	if t.current >= len(t.source) {
		return 0
	}
	return t.source[t.current]
}

func (t *Tokenizer) peekNextChar() byte {
	// Need to ensure we aren't passing EOF before calling
	if t.current+1 >= len(t.source) {
		return 0
	}
	return t.source[t.current+1]
}

func (t *Tokenizer) peekMatches(s string) bool {
	if t.current+len(s) > len(t.source) {
		return false
	}
	return t.source[t.current:t.current+len(s)] == s
}

func (t *Tokenizer) peekMatchesOneOf(ss ...string) bool {
	return slices.ContainsFunc(ss, t.peekMatches)
}

func (t *Tokenizer) scanString() error {
	for c := t.peekChar(); c != '"'; c = t.peekChar() {
		if t.current >= len(t.source) {
			return &LoxError{
				line:    t.line,
				where:   "",
				message: "Unterminated string.",
			}
		}
		if c == '\n' {
			t.line++
		}
		t.current++
	}
	// Consume closing quote
	t.current++

	str := t.source[t.start+1 : t.current-1] // +1 and -1 remove the surrounding ""
	t.addToken(STRING, str)

	return nil
}

func (t *Tokenizer) scanNumber() error {
	for isDigit(t.peekChar()) {
		t.current++
	}
	if t.peekChar() == '.' && isDigit(t.peekNextChar()) {
		t.current++ // consume the .
		for isDigit(t.peekChar()) {
			t.current++
		}
	}

	numStr := t.source[t.start:t.current]
	number, err := strconv.ParseFloat(numStr, 64)
	if err != nil {
		return &LoxError{
			line:    t.line,
			where:   "",
			message: "Invalid number.",
		}
	}
	t.addToken(NUMBER, number)

	return nil
}

func (t *Tokenizer) scanIdentifierOrKeyword() {
	for isAlphaNumeric(t.peekChar()) {
		t.current++
	}
	text := t.source[t.start:t.current]
	typ, ok := keywords[text]
	if !ok {
		typ = IDENTIFIER
	}
	t.addToken(typ, nil)
}

func (t *Tokenizer) scanComment() error {
	// Don't add any tokens for comments!
	srcLen := len(t.source)
	// Block comment #[ ... ]#
	if t.current < srcLen && t.peekAndConsume("[") {
		terminated := t.peekAndConsume("]#")
		for t.current < srcLen && !terminated {
			if t.source[t.current] == '\n' {
				t.line++
			}

			t.current++
			terminated = t.peekAndConsume("]#")
		}
		if !terminated {
			return &LoxError{
				line:    t.line,
				where:   "",
				message: "Unterminated #[ comment.",
			}
		}
	} else { // Single line comment # ...
		for t.current < srcLen && t.peekChar() != '\n' {
			t.current++
		}
	}
	return nil
}
