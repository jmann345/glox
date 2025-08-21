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

// NOTE: Right now there is no "Interpreter" struct
// As I do not yet need to keep track of the state of the interpreter
// I prefer to avoid adding additional global state unless absolutely necessary

func Interpret(exprs ...Expr) ([]any, []error) {
	vals, errs := []any{}, []error{}
	for _, expr := range exprs {
		val, err := evaluate(expr)
		vals = append(vals, val)
		errs = append(errs, err)
	}
	return vals, errs
}

func evaluate(expr Expr) (any, error) {
	switch e := expr.(type) {
	case *Literal:
		return e.value, nil
	case *Unary:
		return evalUnary(e)
	case *Binary:
		return evalBinary(e)
	case *IfExpr:
		return evalIfExpr(e)
	case *Grouping:
		return evaluate(e.expression)
	default:
		panic(fmt.Sprintf(
			"Unimplemented Expression type: %T", e))
	}
}

func evalUnary(expr *Unary) (any, error) {
	rhs, err := evaluate(expr.rhs)
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
// evalMath (term or factor)
// <replace with rest>
func evalBinary(expr *Binary) (any, error) {
	switch expr.op.typ {
	case COMMA:
		// discard lhs, return rhs
		_, err := evaluate(expr.lhs)
		if err != nil {
			return nil, err
		}

		rhs, err := evaluate(expr.rhs)
		if err != nil {
			return nil, err
		}

		return rhs, nil
	case EQUAL_EQUAL, BANG_EQUAL:
		return evalEquality(expr)
	case LESS, LESS_EQUAL, GREATER, GREATER_EQUAL:
		return evalComparison(expr)
	case PLUS, MINUS, STAR, SLASH:
		return evalMath(expr)
	}
	panic("Unreachable.")
}

func evalEquality(expr *Binary) (any, error) {
	lhs, err := evaluate(expr.lhs)
	if err != nil {
		return nil, err
	}

	rhs, err := evaluate(expr.rhs)
	if err != nil {
		return nil, err
	}

	if !SameType(lhs, rhs) {
		if lhs == nil || rhs == nil {
			switch expr.op.typ {
			case EQUAL_EQUAL:
				return false, nil
			case BANG_EQUAL:
				return true, nil
			default:
				panic("Unreachable.")
			}
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
	lhs, err := evaluate(expr.lhs)
	if err != nil {
		return nil, err
	}

	rhs, err := evaluate(expr.rhs)
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

func evalMath(expr *Binary) (any, error) {
	lhs, err := evaluate(expr.lhs)
	if err != nil {
		return nil, err
	}

	rhs, err := evaluate(expr.rhs)
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

	// Currently, the only overloaded op is '+' for string concatenation
	if lhs, ok := lhs.(string); ok {
		if expr.op.typ != PLUS {
			return nil, &RuntimeError{
				expr.op,
				fmt.Sprintf("'%s' Operands must be numbers.", expr.op.lexeme),
			}
		}

		rhs, _ := rhs.(string)
		return lhs + rhs, nil
	}

	lhs_n, ok := lhs.(float64)
	rhs_n, _ := rhs.(float64)
	if !ok {
		return nil, &RuntimeError{
			expr.op,
			fmt.Sprintf("Invalid math operand type: %T", lhs),
		}
	}

	switch expr.op.typ {
	case PLUS:
		return lhs_n + rhs_n, nil
	case MINUS:
		return lhs_n - rhs_n, nil
	case STAR:
		return lhs_n * rhs_n, nil
	case SLASH:
		// NOTE: golang behavior:
		// 0/0 == NaN
		// 1/0 == +Inf
		// -1/0 == -Inf
		// I'm fine with this for now!
		return lhs_n / rhs_n, nil
	}

	panic("Unreachable.")
}

func evalIfExpr(expr *IfExpr) (any, error) {
	cond, err := evaluate(expr.cond)
	if err != nil {
		return nil, err
	}

	condVal, ok := cond.(bool)
	if !ok {
		return nil, &RuntimeError{
			expr.tok,
			"Condition of 'if' must be boolean.",
		}
	}

	// NOTE: For now, an if expression can evaluate to different types
	// NOTE: Error's in the non-traveled branch won't be reported
	if condVal {
		return evaluate(expr.thenBranch)
	} else {
		return evaluate(expr.elseBranch)
	}
}
