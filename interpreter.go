package main

import "fmt"

type RuntimeError struct {
	tok Token
	msg string
}

func (e RuntimeError) Error() string {
	return fmt.Sprintf("[line %d] RuntimeError at '%s': %s",
		e.tok.line, e.tok.lexeme, e.msg)
}

type Interpreter struct {
	globals *Environment
	env     *Environment
	locals  map[Expr]int
}

func NewInterpreter() *Interpreter {
	globals := NewEnvironment(nil)
	globals.Set("clock", Clock{})
	globals.Set("len", Len{})
	globals.Set("append", Append{})
	globals.Set("type", Type{})

	return &Interpreter{globals, globals, make(map[Expr]int)}
}

func (i *Interpreter) Resolve(expr Expr, depth int) {
	i.locals[expr] = depth
}

func (i *Interpreter) Interpret(stmt Stmt) error {
	return i.execute(stmt)
}

func (i *Interpreter) execute(stmt Stmt) error {
	switch s := stmt.(type) {
	case *VarDecl:
		val, err := i.evaluate(s.initializer)
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
	case *FunDecl:
		i.env.Set(s.name.lexeme, &Function{
			decl: s, closure: i.env, isInitializer: false,
		})

		return nil
	case *ClassDecl:
		i.env.Set(s.name.lexeme, nil)

		methods := make(map[string]*Function)
		for _, meth := range s.methods {
			method, ok := meth.(*FunDecl)
			if !ok {
				panic("Unreachable.")
			}

			methods[method.name.lexeme] = &Function{
				decl:          method,
				closure:       i.env,
				isInitializer: method.name.lexeme == "init",
			}
		}

		staticMethods := make(map[string]*Function)
		for _, meth := range s.staticMethods {
			method, ok := meth.(*FunDecl)
			if !ok {
				panic("Unreachable.")
			}

			staticMethods[method.name.lexeme] = &Function{
				decl:          method,
				closure:       i.env,
				isInitializer: false,
			}
		}

		class := &Class{s.name.lexeme, methods, staticMethods}
		i.env.SetInScope(s.name.lexeme, class)

		return nil
	case *Block:
		return i.execBlock(s, NewEnvironment(i.env))
	case *IfStmt:
		return i.execIfStmt(s)
	case *WhileStmt:
		return i.execWhileStmt(s)
	case *ForStmt:
		return i.execForStmt(s)
	case *BreakStmt:
		return Break{}
	case *CycleStmt:
		return Cycle{}
	case *ReturnStmt:
		return i.execReturnStmt(s)
	case *NoOpStmt: // no-op
		return nil
	default:
		panic(fmt.Sprintf("Unimplemented Statement type: %T", s))
	}
}

func (i *Interpreter) execBlock(block *Block, env *Environment) error {
	prevEnv := i.env

	i.env = env
	defer func() { i.env = prevEnv }() // ensure restoration even on error

	for _, stmt := range block.stmts {
		if err := i.execute(stmt); err != nil {
			return err
		}
	}

	return nil
}

func (i *Interpreter) execIfStmt(stmt *IfStmt) error {
	condVal, err := i.evaluate(stmt.condition)
	if err != nil {
		return err
	}

	condValBool, ok := condVal.(bool)
	if !ok {
		return RuntimeError{
			stmt.token,
			"Condition of 'if' must be boolean.",
		}
	}

	if condValBool {
		err := i.execute(stmt.thenBranch)
		return err
	}

	if stmt.elseBranch != nil {
		err := i.execute(stmt.elseBranch)
		return err
	}

	return nil
}

func (i *Interpreter) execWhileStmt(stmt *WhileStmt) error {
	condVal, err := i.evaluate(stmt.condition)
	if err != nil {
		return err
	}

	condValBool, ok := condVal.(bool)
	if !ok {
		return RuntimeError{
			stmt.token,
			"Condition of 'while' must be boolean.",
		}
	}

whileLoop:
	for condValBool {
		if err = i.execute(stmt.body); err != nil {
			switch err.(type) {
			case Break:
				break whileLoop
			case Cycle:
				continue whileLoop
			default:
				return err
			}
		}

		condVal, err = i.evaluate(stmt.condition)
		if err != nil {
			return err
		}

		condValBool, ok = condVal.(bool)
		if !ok { // With a better type system, we wouldn't have to check this every time!
			return RuntimeError{
				stmt.token,
				"Condition of 'while' must be boolean.",
			}
		}
	}

	return nil
}

func (i *Interpreter) execForStmt(stmt *ForStmt) error {
	prevEnv := i.env

	// make new env so the initializer doesn't exist out of scope
	i.env = NewEnvironment(prevEnv)
	defer func() { i.env = prevEnv }()

	if err := i.execute(stmt.initializer); err != nil {
		return err
	}

forLoop:
	for {
		condVal, err := i.evaluate(stmt.condition)
		if err != nil {
			return err
		}

		if _, ok := stmt.condition.(*NoOpExpr); !ok {
			condValBool, ok := condVal.(bool)
			if !ok {
				return RuntimeError{
					stmt.token,
					"Condition of 'for' must be boolean.",
				}
			}

			if !condValBool {
				break
			}
		}

		if err = i.execute(stmt.body); err != nil {
			switch err.(type) {
			case Break:
				break forLoop
			case Cycle:
				continue forLoop
			default:
				return err
			}
		}

		if _, err = i.evaluate(stmt.increment); err != nil {
			return err
		}
	}

	return nil
}

func (i *Interpreter) execReturnStmt(stmt *ReturnStmt) error {
	var value any = nil
	var err error = nil

	if stmt.value != nil {
		value, err = i.evaluate(stmt.value)
	}

	if err != nil {
		return err
	}

	return Return{value}
}

func (i *Interpreter) evaluate(expr Expr) (any, error) {
	switch e := expr.(type) {
	case *Literal:
		return e.value, nil
	case *Variable:
		return i.lookUpVariable(e.name, expr)
	case *Assign:
		return i.assignVariable(e.name, e.value, expr)
	case *Unary:
		return i.evalUnary(e)
	case *Postfix:
		return i.evalPostfix(e)
	case *Binary:
		return i.evalBinary(e)
	case *CallExpr:
		return i.evalCall(e)
	case *Index:
		return i.evalIndex(e)
	case *List:
		return i.evalList(e)
	case *Get:
		return i.evalGet(e)
	case *Set:
		return i.evalSet(e)
	case *SetIndex:
		return i.evalSetIndex(e)
	case *AugSet:
		return i.evalAugSet(e)
	case *AugSetIndex:
		return i.evalAugSetIndex(e)
	case *Ternary:
		return i.evalTernary(e)
	case *This:
		return i.lookUpVariable(e.keyword, expr)
	case *Grouping:
		return i.evaluate(e.expression)
	case *FunExpr:
		return &AnonFunction{expr: e, closure: i.env}, nil
	case *NoOpExpr:
		return nil, nil
	default:
		panic(fmt.Sprintf("Unimplemented Expression type: %T", e))
	}
}

func (i *Interpreter) lookUpVariable(name Token, expr Expr) (any, error) {
	if distance, ok := i.locals[expr]; ok {
		return i.env.GetAt(distance, name.lexeme), nil
	}

	val, ok := i.globals.Get(name.lexeme)
	if !ok {
		return nil, RuntimeError{
			name, "Undefined Variable '" + name.lexeme + "'.",
		}
	}

	return val, nil
}

func (i *Interpreter) assignVariable(
	name Token, value Expr, expr Expr,
) (any, error) {
	val, err := i.evaluate(value)
	if err != nil {
		return nil, err
	}

	distance, ok := i.locals[expr]
	if ok {
		i.env.SetAt(distance, name.lexeme, val)
	} else {
		i.globals.Set(name.lexeme, val)
	}

	return val, nil
}

func (i *Interpreter) evalUnary(expr *Unary) (any, error) {
	// NOTE: I chose to only make 'false' falsey
	switch expr.op.typ {
	case NOT:
		value, err := i.evaluate(expr.rhs)
		if err != nil {
			return nil, err
		}

		val, ok := value.(bool)
		if !ok {
			return nil, RuntimeError{
				tok: expr.op,
				msg: "'not' operand must be boolean.",
			}
		}

		return !val, nil
	case MINUS:
		value, err := i.evaluate(expr.rhs)
		if err != nil {
			return nil, err
		}

		val, ok := value.(float64)
		if !ok {
			return nil, RuntimeError{
				tok: expr.op,
				msg: "'-' operand must be a number",
			}
		}

		return -val, nil
	default: // ++/--: Update the value of the variable, return the new value
		switch e := expr.rhs.(type) {
		case *Variable:
			rhs, err := i.lookUpVariable(e.name, e)
			if err != nil {
				return nil, err
			}

			val, ok := rhs.(float64)
			if !ok {
				return nil, RuntimeError{
					tok: expr.op,
					msg: "'" + expr.op.lexeme + "' operand must be a number",
				}
			}

			switch expr.op.typ {
			case MINUS_MINUS:
				val--
			case PLUS_PLUS:
				val++
			default:
				panic("Unreachable.")
			}

			i.env.SetInScope(e.name.lexeme, val)
			return val, nil
		case *Index:
			list, err := i.evaluate(e.list)
			if err != nil {
				return nil, err
			}

			lst, ok := list.([]any)
			if !ok {
				return nil, RuntimeError{
					e.bracket,
					"Subscripted value is not a list.",
				}
			}

			index, err := i.evaluate(e.index)
			if err != nil {
				return nil, err
			}

			var idx int
			if index, ok := index.(float64); !ok {
				return nil, RuntimeError{
					e.bracket,
					"List subscript is not a number.",
				}
			} else {
				idx = int(index)
			}

			if idx < 0 || idx >= len(lst) {
				return nil, RuntimeError{
					e.bracket,
					fmt.Sprintf(
						"Index out of range [%v] with length %v.",
						idx, len(lst),
					),
				}
			}

			val, ok := lst[idx].(float64)
			if !ok {
				return nil, RuntimeError{
					tok: expr.op,
					msg: "'" + expr.op.lexeme + "' operand must be a number",
				}
			}

			switch expr.op.typ {
			case MINUS_MINUS:
				val--
			case PLUS_PLUS:
				val++
			default:
				panic("Unreachable.")
			}

			lst[idx] = val
			return val, nil
		case *Get:
			object, err := i.evaluate(e.object)
			if err != nil {
				return nil, err
			}

			obj, ok := object.(Object)
			if !ok {
				return nil, RuntimeError{expr.op,
					"'" + expr.op.lexeme + "' target must be an object property",
				}
			}

			value, err := obj.Get(e.name)
			if err != nil {
				return nil, err
			}

			val, ok := value.(float64)
			if !ok {
				return nil, RuntimeError{
					tok: expr.op,
					msg: "'" + expr.op.lexeme + "' operand must be a number",
				}
			}

			switch expr.op.typ {
			case MINUS_MINUS:
				val--
			case PLUS_PLUS:
				val++
			default:
				panic("Unreachable.")
			}

			if err := obj.Set(e.name, val); err != nil {
				return nil, err
			}

			return val, nil
		default:
			return nil, RuntimeError{
				tok: expr.op,
				msg: "Expression is not assignable",
			}
		}
	}
}

func (i *Interpreter) evalPostfix(expr *Postfix) (any, error) {
	switch e := expr.lhs.(type) {
	case *Variable:
		lhs, err := i.lookUpVariable(e.name, e)
		if err != nil {
			return nil, err
		}

		val, ok := lhs.(float64)
		if !ok {
			return nil, RuntimeError{
				tok: expr.op,
				msg: "'" + expr.op.lexeme + "' operand must be a number",
			}
		}

		switch expr.op.typ {
		case MINUS_MINUS:
			i.env.SetInScope(e.name.lexeme, val-1)
		case PLUS_PLUS:
			i.env.SetInScope(e.name.lexeme, val+1)
		default:
			panic("Unreachable.")
		}

		return val, nil
	case *Index:
		list, err := i.evaluate(e.list)
		if err != nil {
			return nil, err
		}

		lst, ok := list.([]any)
		if !ok {
			return nil, RuntimeError{
				e.bracket,
				"Subscripted value is not a list.",
			}
		}

		index, err := i.evaluate(e.index)
		if err != nil {
			return nil, err
		}

		var idx int
		if index, ok := index.(float64); !ok {
			return nil, RuntimeError{
				e.bracket,
				"List subscript is not a number.",
			}
		} else {
			idx = int(index)
		}

		if idx < 0 || idx >= len(lst) {
			return nil, RuntimeError{
				e.bracket,
				fmt.Sprintf(
					"Index out of range [%v] with length %v.",
					idx, len(lst),
				),
			}
		}

		val, ok := lst[idx].(float64)
		if !ok {
			return nil, RuntimeError{
				tok: expr.op,
				msg: "'" + expr.op.lexeme + "' operand must be a number",
			}
		}

		switch expr.op.typ {
		case MINUS_MINUS:
			lst[idx] = val - 1
		case PLUS_PLUS:
			lst[idx] = val + 1
		default:
			panic("Unreachable.")
		}

		return val, nil
	case *Get:
		object, err := i.evaluate(e.object)
		if err != nil {
			return nil, err
		}

		obj, ok := object.(Object)
		if !ok {
			return nil, RuntimeError{expr.op,
				"'" + expr.op.lexeme + "' target must be an object property",
			}
		}

		value, err := obj.Get(e.name)
		if err != nil {
			return nil, err
		}

		val, ok := value.(float64)
		if !ok {
			return nil, RuntimeError{
				tok: expr.op,
				msg: "'" + expr.op.lexeme + "' operand must be a number",
			}
		}

		switch expr.op.typ {
		case MINUS_MINUS:
			if err := obj.Set(e.name, val-1); err != nil {
				return nil, err
			}
		case PLUS_PLUS:
			if err := obj.Set(e.name, val+1); err != nil {
				return nil, err
			}
		default:
			panic("Unreachable.")
		}

		return val, nil
	default:
		return nil, RuntimeError{
			tok: expr.op,
			msg: "Expression is not assignable",
		}
	}
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
	default:
		panic("Unreachable.")
	}
}

func (i *Interpreter) evalCall(expr *CallExpr) (any, error) {
	if v, ok := expr.callee.(*Variable); ok {
		if _, exists := i.env.Get(v.name.lexeme); !exists {
			return nil, RuntimeError{v.name,
				"Call to undeclared function '" + v.name.lexeme + "'",
			}
		}
	}

	callee, err := i.evaluate(expr.callee)
	if err != nil {
		return nil, err
	}

	args := []any{}
	for _, arg := range expr.arguments {
		argument, err := i.evaluate(arg)
		if err != nil {
			return nil, err
		}

		args = append(args, argument)
	}

	function, ok := callee.(Callable)
	if !ok {
		errMsg := "Can only call functions and classes."
		switch callee.(type) {
		case nil:
			errMsg = "Called object type 'nil' is not a function."
		case bool:
			errMsg = "Called object type 'boolean' is not a function."
		case float64:
			errMsg = "Called object type 'number' is not a function."
		case string:
			errMsg = "Called object type 'string' is not a function."
		case []any:
			errMsg = "Called object type 'list' is not a function."
		default:
			panic("Unreachable.") // fallback in case I add more types later
		}
		return nil, RuntimeError{expr.paren, errMsg}
	}

	if arity := function.Arity(); len(args) != arity {
		return nil, RuntimeError{expr.paren, fmt.Sprintf(
			"Expected %d arguments but got %d.", arity, len(args),
		)}
	}

	val := function.Call(i, args)
	if err, ok := val.(error); ok {
		return nil, err
	}

	return val, nil
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

		return nil, RuntimeError{
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
		return nil, RuntimeError{
			tok: expr.op,
			msg: fmt.Sprintf(
				"Incompatible types: %T and %T", lhs, rhs,
			),
		}
	}

	switch lhs.(type) {
	case nil:
		return nil, RuntimeError{
			tok: expr.op,
			msg: "Cannot use '" + expr.op.lexeme + "' with nil.",
		}
	case bool:
		return nil, RuntimeError{
			tok: expr.op,
			msg: "Cannot use '" + expr.op.lexeme + "' with boolean values.",
		}
	// TODO: Allow string comparison!
	case string:
		return nil, RuntimeError{
			tok: expr.op,
			msg: "Cannot use '" + expr.op.lexeme + "' with string values.",
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

	return DoMath(expr.op, lhs, rhs)
}

func (i *Interpreter) evalLogical(expr *Binary) (any, error) {
	lhs, err := i.evaluate(expr.lhs)
	if err != nil {
		return nil, err
	}

	lhs_b, ok := lhs.(bool)
	if !ok {
		return nil, RuntimeError{
			expr.op,
			fmt.Sprintf("Invalid logical operand type: %T", lhs),
		}
	}

	// Try to short circuit
	if expr.op.typ == OR && lhs_b {
		return true, nil 
	}

	if expr.op.typ == AND && !lhs_b {
		return false, nil
	}

	rhs, err := i.evaluate(expr.rhs)
	if err != nil {
		return nil, err
	}

	rhs_b, ok := rhs.(bool)
	if !ok {
		return nil, RuntimeError{
			expr.op,
			fmt.Sprintf("Invalid logical operand type: %T", rhs),
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

func (i *Interpreter) evalIndex(expr *Index) (any, error) {
	list, err := i.evaluate(expr.list)
	if err != nil {
		return nil, err
	}

	lst, ok := list.([]any)
	if !ok {
		return nil, RuntimeError{
			expr.bracket,
			"Subscripted value is not a list.",
		}
	}

	index, err := i.evaluate(expr.index)
	if err != nil {
		return nil, err
	}

	var idx int
	if index, ok := index.(float64); !ok {
		return nil, RuntimeError{
			expr.bracket,
			"List subscript is not a number.",
		}
	} else {
		idx = int(index)
	}

	if idx < 0 || idx >= len(lst) {
		return nil, RuntimeError{
			expr.bracket,
			fmt.Sprintf(
				"Index out of range [%v] with length %v.",
				idx, len(lst),
			),
		}
	}

	return lst[idx], nil
}

func (i *Interpreter) evalList(expr *List) ([]any, error) {
	lst := []any{}
	for _, value := range expr.values {
		val, err := i.evaluate(value)
		if err != nil {
			return nil, err
		}

		lst = append(lst, val)
	}

	return lst, nil
}

func (i *Interpreter) evalGet(expr *Get) (any, error) {
	object, err := i.evaluate(expr.object)
	if err != nil {
		return nil, err
	}

	if obj, ok := object.(Object); ok {
		return obj.Get(expr.name)
	}

	return nil, RuntimeError{expr.name, "Only objects have properties."}
}

func (i *Interpreter) evalSet(expr *Set) (any, error) {
	object, err := i.evaluate(expr.object)
	if err != nil {
		return nil, err
	}

	obj, ok := object.(Object)
	if !ok {
		return nil, RuntimeError{expr.name, "Only objects have fields."}
	}

	value, err := i.evaluate(expr.value)
	if err != nil {
		return nil, err
	}

	if err := obj.Set(expr.name, value); err != nil {
		return nil, err
	}

	return value, nil
}

func (i *Interpreter) evalSetIndex(expr *SetIndex) (any, error) {
	list, err := i.evaluate(expr.list)
	if err != nil {
		return nil, err
	}

	lst, ok := list.([]any)
	if !ok {
		return nil, RuntimeError{
			expr.bracket,
			"Subscripted value is not a list.",
		}
	}

	index, err := i.evaluate(expr.index)
	if err != nil {
		return nil, err
	}

	var idx int
	if index, ok := index.(float64); !ok {
		return nil, RuntimeError{
			expr.bracket,
			"List subscript is not a number.",
		}
	} else {
		idx = int(index)
	}

	if idx < 0 || idx >= len(lst) {
		return nil, RuntimeError{
			expr.bracket,
			fmt.Sprintf(
				"Index out of range [%v] with length %v.",
				idx, len(lst),
			),
		}
	}

	value, err := i.evaluate(expr.value)
	if err != nil {
		return nil, err
	}

	lst[idx] = value

	return value, nil
}

func (i *Interpreter) evalAugSet(expr *AugSet) (any, error) {
	object, err := i.evaluate(expr.object)
	if err != nil {
		return nil, err
	}

	obj, ok := object.(Object); 
	if !ok {
		return nil, RuntimeError{expr.name, "Only objects have fields."}
	}
	
	rhs, err := i.evaluate(expr.rhs)
	if err != nil {
		return nil, err
	}

	val, err := obj.Get(expr.name)
	if err != nil {
		return nil, err
	}

	newVal, err := DoMath(expr.op, val, rhs)
	if err != nil {
		return nil, err
	}

	if err := obj.Set(expr.name, newVal); err != nil {
		return nil, err
	}

	return newVal, nil
}

func (i *Interpreter) evalAugSetIndex(expr *AugSetIndex) (any, error) {
	list, err := i.evaluate(expr.list)
	if err != nil {
		return nil, err
	}

	lst, ok := list.([]any)
	if !ok {
		return nil, RuntimeError{
			expr.bracket,
			"Subscripted value is not a list.",
		}
	}

	index, err := i.evaluate(expr.index)
	if err != nil {
		return nil, err
	}

	var idx int
	if index, ok := index.(float64); !ok {
		return nil, RuntimeError{
			expr.bracket,
			"List subscript is not a number.",
		}
	} else {
		idx = int(index)
	}

	if idx < 0 || idx >= len(lst) {
		return nil, RuntimeError{
			expr.bracket,
			fmt.Sprintf(
				"Index out of range [%v] with length %v.",
				idx, len(lst),
			),
		}
	}

	rhs, err := i.evaluate(expr.rhs)
	if err != nil {
		return nil, err
	}

	newValue, err := DoMath(expr.op, lst[idx], rhs)
	if err != nil {
		return nil, err
	}

	lst[idx] = newValue

	return newValue, nil
}

func (i *Interpreter) evalTernary(expr *Ternary) (any, error) {
	cond, err := i.evaluate(expr.condition)
	if err != nil {
		return nil, err
	}

	condVal, ok := cond.(bool)
	if !ok {
		return nil, RuntimeError{
			expr.token,
			"Condition of ternary must be boolean.",
		}
	}

	if condVal {
		return i.evaluate(expr.trueBranch)
	} else {
		return i.evaluate(expr.falseBranch)
	}
}

