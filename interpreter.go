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
	env *Environment
}

func NewInterpreter() *Interpreter {
	return &Interpreter{NewEnvironment(nil)}
}

func (i *Interpreter) Interpret(stmts ...Stmt) []error {
	errs := []error{}
	for _, stmt := range stmts {
		err := i.execute(stmt)
		errs = append(errs, err)
	}
	return errs
}

func (i *Interpreter) execute(stmt Stmt) error {
	switch s := stmt.(type) {
	case *VarDecl:
		val, err := i.evaluate(s.expr)
		if err != nil {
			return err
		}

		i.env.Set(s.name.lexeme, val)

		return nil
	case *ExprStmt:
		_, err := i.evaluate(s.expr)
		return err
	case *PrintStmt:
		value, err := i.evaluate(s.expr)
		if err != nil {
			return err
		}

		fmt.Println(Stringify(value))

		return nil
	case *IfStmt:
		return i.execIfStmt(*s)
	case *WhileStmt:
		return i.execWhileStmt(*s)
	case *ForStmt:
		return i.execForStmt(*s)
	case *Block:
		prevEnv := i.env

		i.env = NewEnvironment(prevEnv)
		defer func() { i.env = prevEnv }()  // ensure restoration even on error

		for _, stmt := range s.stmts {
			err := i.execute(stmt)
			if err != nil {
				return err
			}
		}

		i.env = prevEnv

		return nil
	case nil: // no-op
		return nil
	default:
		panic(fmt.Sprintf(
			"Unimplemented Statement type: %T", s))
	}
}

func (i *Interpreter) execIfStmt(stmt IfStmt) error {
	condVal, err := i.evaluate(stmt.condition)
	if err != nil {
		return err
	}

	condValBool, ok := condVal.(bool); 
	if !ok {
		return &RuntimeError{
			stmt.token,
			"Condition of 'if' must be boolean.",
		}
	}

	if condValBool {
		return i.execute(stmt.thenBranch)
	} 

	if stmt.elseBranch != nil {
		return i.execute(stmt.elseBranch)
	}

	return nil
}

func (i *Interpreter) execWhileStmt(stmt WhileStmt) error {
	condVal, err := i.evaluate(stmt.condition)
	if err != nil {
		return err
	}

	condValBool, ok := condVal.(bool); 
	if !ok {
		return &RuntimeError{
			stmt.token,
			"Condition of 'while' must be boolean.",
		}
	}

	for condValBool {
		if err = i.execute(stmt.body); err != nil {
			return err
		}

		condVal, err = i.evaluate(stmt.condition)
		if err != nil {
			return err
		}

		condValBool, ok = condVal.(bool); 
		if !ok { // With a better type system, we wouldn't have to check this every time!
			return &RuntimeError{
				stmt.token,
				"Condition of 'while' must be boolean.",
			}
		}
	} 

	return nil
}

func (i *Interpreter) execForStmt(stmt ForStmt) error {
	prevEnv := i.env

	// make new env so the initializer doesn't exist out of scope
	i.env = NewEnvironment(prevEnv)
	defer func() { i.env = prevEnv }()

	if err := i.execute(stmt.initializer); err != nil {
		return err
	}

	for {
		condVal, err := i.evaluate(stmt.condition)
		if err != nil {
			return err
		}

		condValBool, ok := condVal.(bool); 
		if !ok {
			return &RuntimeError{
				stmt.token,
				"Condition of 'for' must be boolean.",
			}
		}

		if !condValBool {
			break
		}

		if err = i.execute(stmt.body); err != nil {
			return err
		}

		if _, err = i.evaluate(stmt.increment); err != nil {
			return err
		}
	} 

	return nil
}

func (i *Interpreter) evaluate(expr Expr) (any, error) {
	switch e := expr.(type) {
	case *Literal:
		return e.value, nil
	case *Variable:
		val, ok := i.env.Get(e.name.lexeme)
		if !ok {
			return nil, &RuntimeError{
				e.name, "Undefined Variable '" + e.name.lexeme + "'.",
			}
		}

		return val, nil
	case *Assign:
		if _, ok := i.env.Get(e.name.lexeme); !ok {
			return nil, &RuntimeError{
				e.name, "Undefined Variable '" + e.name.lexeme + "'.",
			}
		}

		val, err := i.evaluate(e.value)
		if err != nil {
			return nil, err
		}

		i.env.SetInScope(e.name.lexeme, val)

		return val, nil // not sure if we need to return e.value
	case *Unary:
		return i.evalUnary(e)
	case *Postfix:
		return i.evalPostfix(e)
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
				msg: "'-' Operand must be a number",
			}
		}

		return -rhs, nil
	// ++/--: Update the value of the variable, return the new value
	case MINUS_MINUS:
		// NOTE:
		// Using expr.rhs here is a bit confusing
		// NOT and MINUS need to evaluate rhs and check the type
		// But the increment/decrement operators need to both
		// Check that the rhs is a VARIABLE, AND THEN check
		// if it evaluates to a number
		// TODO: This would be much nicer with a proper type system!
		varExpr, ok := expr.rhs.(*Variable)
		if !ok {
			return nil, &RuntimeError{
				tok: expr.op,
				msg: "Expression is not assignable",
			}
		}

		key := varExpr.name.lexeme
		_, ok = i.env.Get(key)
		if !ok {
			return nil, &RuntimeError{
				varExpr.name,
				"Use of undeclared identifier '" + key + "'",
			}
		}

		val, ok := rhs.(float64)
		if !ok {
			return nil, &RuntimeError{
				tok: expr.op,
				msg: "'--' Operand must be a number",
			}
		}

		i.env.SetInScope(key, val-1)

		return val - 1, nil
	case PLUS_PLUS:
		varExpr, ok := expr.rhs.(*Variable)
		if !ok {
			return nil, &RuntimeError{
				tok: expr.op,
				msg: "Expression is not assignable",
			}
		}

		key := varExpr.name.lexeme
		_, ok = i.env.Get(key)
		if !ok {
			return nil, &RuntimeError{
				varExpr.name,
				"Use of undeclared identifier '" + key + "'",
			}
		}

		val, ok := rhs.(float64)
		if !ok {
			return nil, &RuntimeError{
				tok: expr.op,
				msg: "'++' Operand must be a number",
			}
		}

		i.env.SetInScope(key, val+1)

		return val + 1, nil
	default:
		panic(fmt.Sprintf(
			"Unreachable: unexpected unary operand: %v", expr.op))
	}
}

func (i *Interpreter) evalPostfix(expr *Postfix) (any, error) {
	lhs, err := i.evaluate(expr.lhs)
	if err != nil {
		return nil, err
	}

	ident, ok := expr.lhs.(*Variable)
	if !ok {
		return nil, &RuntimeError{
			tok: expr.op,
			msg: "Expression is not assignable",
		}
	}

	key := ident.name.lexeme
	_, ok = i.env.Get(key)
	if !ok {
		return nil, &RuntimeError{
			ident.name,
			"Use of undeclared identifier '" + key + "'",
		}
	}

	val, ok := lhs.(float64)
	if !ok {
		return nil, &RuntimeError{
			tok: expr.op,
			msg: "'" + expr.op.lexeme + "' Operand must be a number",
		}
	}

	switch expr.op.typ {
	case MINUS_MINUS:
		i.env.SetInScope(key, val-1)
	case PLUS_PLUS:
		i.env.SetInScope(key, val+1)
	default:
		panic("Unreachable.")
	}

	return val, nil
}

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
	case AND, OR:
		return i.evalLogical(expr)
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
				fmt.Sprintf(
					"'%s' Operands must be both be numbers or strings.",
					expr.op.lexeme,
				),
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

func (i *Interpreter) evalLogical(expr *Binary) (any, error) {
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

	lhs_b, ok := lhs.(bool)
	rhs_b, _ := rhs.(bool)
	if !ok {
		return nil, &RuntimeError{
			expr.op,
			fmt.Sprintf("Invalid math operand type: %T", lhs),
		}
	}

	switch expr.op.typ {
	case OR:
		return lhs_b || rhs_b, nil
	case AND:
		return lhs_b && rhs_b, nil
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
