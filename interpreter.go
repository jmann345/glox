package main

import "fmt"

type RuntimeError struct {
	tok Token
	msg string
}

func (e *RuntimeError) Error() string {
	return fmt.Sprintf("[line %d] RuntimeError at '%s': %s",
		e.tok.line, e.tok.lexeme, e.msg)
}

func Evaluate(expr Expr) (any, error) {
	switch e := expr.(type) {
	case *Literal:
		return e.value, nil
	case *Unary:
		return evalUnary(e)
	case *Binary:
		return evalBinary(e)
	default:
		panic(fmt.Sprintf(
			"Unimplemented Expression type: %T", e))
	}
}

func evalUnary(expr *Unary) (any, error) {
	rhs, err := Evaluate(expr.rhs)
	if err != nil {
		return nil, err
	}
	// Difference: I chose to only make 'false' falsey
	// The book makes 'nil' falsely, but I don't like that
	switch expr.op.typ {
	case NOT:
		rhs, ok := rhs.(bool)
		if !ok {
			return nil, &RuntimeError{
				tok: expr.op,
				msg: "'not' Operand must be boolean.",
			}
		}

		return !rhs, nil
	case MINUS:
		rhs, ok := rhs.(float64)
		if !ok {
			return nil, &RuntimeError{
				tok: expr.op,
				msg: "'-' Operand must be a float64",
			}
		}

		return -rhs, nil
	default:
		panic(fmt.Sprintf(
			"Unreachable: unexpected unary operand: %v", expr.op))
	}
}

// TODO: Split into:
// evalComma
// evalEquality
// evalComparison
// evalMath (term or factor) -- HANDLE STRING CONCAT
func evalBinary(expr *Binary) (any, error) {
	// FIXME: Need to handle nils properly
	// For example, "hello" == nil is valid
	// But "hello" + nil is an incompatible type error!
	// Or... we could simply enforce nilable types as separate
	// For now, let's allow everything to be nilable.
	// TODO: Use where relevent
	switch expr.op.typ {
	case COMMA:
		// discard lhs, return rhs
		_, err := Evaluate(expr.lhs)
		if err != nil {
			return nil, err
		}

		rhs, err := Evaluate(expr.rhs)
		if err != nil {
			return nil, err
		}

		return rhs, nil
	case EQUAL_EQUAL, BANG_EQUAL:
		return evalEquality(expr)
	case LESS, LESS_EQUAL, GREATER, GREATER_EQUAL:
		return evalComparison(expr)
	}
	panic("Unreachable.")
}

func evalEquality(expr *Binary) (any, error) {
	lhs, err := Evaluate(expr.lhs)
	if err != nil {
		return nil, err
	}

	rhs, err := Evaluate(expr.rhs)
	if err != nil {
		return nil, err
	}

	if !SameType(lhs, rhs) {
		if lhs == nil || rhs == nil {
			if expr.op.typ == EQUAL_EQUAL {
				return false, nil
			}
			return true, nil
		}

		return nil, &RuntimeError{
			tok: expr.op,
			msg: fmt.Sprintf(
				"Incompatible types: %T and %T", lhs, rhs,
			),
		}
	}

	switch expr.op.typ {
	case EQUAL_EQUAL:
		return lhs == rhs, nil
	case BANG_EQUAL:
		return lhs != rhs, nil
	}

	panic("Unreachable.")
}

func evalComparison(expr *Binary) (any, error) {
	lhs, err := Evaluate(expr.lhs)
	if err != nil {
		return nil, err
	}

	rhs, err := Evaluate(expr.rhs)
	if err != nil {
		return nil, err
	}

	if !SameType(lhs, rhs) {
		return nil, &RuntimeError{
			tok: expr.op,
			msg: fmt.Sprintf(
				"Incompatible types: %T and %T", lhs, rhs,
			),
		}
	}

	switch lhs.(type) {
	case nil:
		return nil, &RuntimeError{
			tok: expr.op,
			msg: "Cannot use " + expr.op.lexeme + " with nil.",
		}
	case bool:
		return nil, &RuntimeError{
			tok: expr.op,
			msg: "Cannot use " + expr.op.lexeme + " with boolean values.",
		}
	// TODO: Allow string comparison!
	case string:
		return nil, &RuntimeError{
			tok: expr.op,
			msg: "Cannot use " + expr.op.lexeme + " with string values.",
		}
	case float64:
		lhs := lhs.(float64)
		rhs := rhs.(float64)
		switch expr.op.typ {
		case LESS:
			return lhs < rhs, nil
		case LESS_EQUAL:
			return lhs <= rhs, nil
		case GREATER:
			return lhs > rhs, nil
		case GREATER_EQUAL:
			return lhs >= rhs, nil
		}
	}

	panic("Unreachable.")
}
