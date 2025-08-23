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

type Interpreter struct {
	env Environment
}

func NewInterpreter() *Interpreter {
	return &Interpreter{
		Environment{nil, make(map[string]any)},
	}
}

func (i *Interpreter) Interpret(stmts ...Stmt) ([]any, []error) {
	vals, errs := []any{}, []error{}
	for _, stmt := range stmts {
		val, err := i.execute(stmt)
		vals = append(vals, val)
		errs = append(errs, err)
	}
	return vals, errs
}

func (i *Interpreter) execute(stmt Stmt) (any, error) {
	switch s := stmt.(type) {
	case *VarDecl:
		val, err := i.evaluate(s.expr)
		if err != nil {
			return nil, err
		}

		i.env.set(s.name.lexeme, val)

		return nil, nil
	case *ExprStmt:
		return i.evaluate(s.expr)
	case *PrintStmt:
		value, err := i.evaluate(s.expr)
		if err != nil {
			return nil, err
		}

		fmt.Println(Stringify(value))

		return nil, nil
	default:
		panic(fmt.Sprintf(
			"Unimplemented Statement type: %T", s))
	}
}

func (i *Interpreter) evaluate(expr Expr) (any, error) {
	switch e := expr.(type) {
	case *Literal:
		return e.value, nil
	case *Variable:
		val, ok := i.env.get(e.name.lexeme)
		if !ok {
			return nil, &RuntimeError{
				e.name, "Undefined Variable '" + e.name.lexeme + "'.",
			}
		}

		return val, nil
	case *Assign:
		if _, ok := i.env.get(e.name.lexeme); !ok {
			return nil, &RuntimeError{
				e.name, "Undefined Variable '" + e.name.lexeme + "'.",
			}
		}

		val, err := i.evaluate(e.value)
		if err != nil {
			return nil, err
		}

		i.env.set(e.name.lexeme, val)

		return val, nil // not sure if we need to return e.value
	case *Unary:
		return i.evalUnary(e)
	case *Binary:
		return i.evalBinary(e)
	case *IfExpr:
		return i.evalIfExpr(e)
	case *Grouping:
		return i.evaluate(e.expression)
	default:
		panic(fmt.Sprintf(
			"Unimplemented Expression type: %T", e))
	}
}

func (i *Interpreter) evalUnary(expr *Unary) (any, error) {
	rhs, err := i.evaluate(expr.rhs)
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
func (i *Interpreter) evalBinary(expr *Binary) (any, error) {
	switch expr.op.typ {
	case COMMA:
		// discard lhs, return rhs
		_, err := i.evaluate(expr.lhs)
		if err != nil {
			return nil, err
		}

		rhs, err := i.evaluate(expr.rhs)
		if err != nil {
			return nil, err
		}

		return rhs, nil
	case EQUAL_EQUAL, BANG_EQUAL:
		return i.evalEquality(expr)
	case LESS, LESS_EQUAL, GREATER, GREATER_EQUAL:
		return i.evalComparison(expr)
	case PLUS, MINUS, STAR, SLASH:
		return i.evalMath(expr)
	}
	panic("Unreachable.")
}

func (i *Interpreter) evalEquality(expr *Binary) (any, error) {
	lhs, err := i.evaluate(expr.lhs)
	if err != nil {
		return nil, err
	}

	rhs, err := i.evaluate(expr.rhs)
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

func (i *Interpreter) evalComparison(expr *Binary) (any, error) {
	lhs, err := i.evaluate(expr.lhs)
	if err != nil {
		return nil, err
	}

	rhs, err := i.evaluate(expr.rhs)
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

func (i *Interpreter) evalMath(expr *Binary) (any, error) {
	lhs, err := i.evaluate(expr.lhs)
	if err != nil {
		return nil, err
	}

	rhs, err := i.evaluate(expr.rhs)
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

func (i *Interpreter) evalIfExpr(expr *IfExpr) (any, error) {
	cond, err := i.evaluate(expr.condition)
	if err != nil {
		return nil, err
	}

	condVal, ok := cond.(bool)
	if !ok {
		return nil, &RuntimeError{
			expr.token,
			"Condition of 'if' must be boolean.",
		}
	}

	// NOTE: For now, an if expression can i.evaluate to different types
	// NOTE: Errors in the non-traveled branch won't be reported
	if condVal {
		return i.evaluate(expr.thenBranch)
	} else {
		return i.evaluate(expr.elseBranch)
	}
}
