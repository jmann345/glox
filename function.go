package main

type Function struct {
	decl          *FunDecl
	closure       *Environment
	isInitializer bool
}

func (f *Function) Bind(instance *Instance) *Function {
	env := NewEnvironment(f.closure)
	env.Set("this", instance)

	return &Function{f.decl, env, f.isInitializer}
}

func (f *Function) Call(interpreter *Interpreter, arguments []any) any {
	localEnv := NewEnvironment(f.closure)
	localEnv.Set(f.decl.name.lexeme, f) // Define function in its own scope so recursion works properly
	for i := 0; i < len(f.decl.params); i++ {
		localEnv.Set(f.decl.params[i].lexeme, arguments[i])
	}

	body, ok := f.decl.body.(*Block)
	if !ok {
		panic("Unreachable.")
	}

	// TODO: See if I can make this less ugly.
	err := interpreter.execBlock(body, localEnv)
	if ret, ok := err.(Return); ok {
		if f.isInitializer {
			return f.closure.GetAt(0, "this")
		}

		return ret.value
	} else if err != nil {
		return err
	}

	if f.isInitializer {
		return f.closure.GetAt(0, "this")
	}

	return nil
}

func (f *Function) Arity() int {
	return len(f.decl.params)
}

func (f *Function) String() string {
	return "<fn " + f.decl.name.lexeme + ">"
}

type AnonFunction struct {
	expr    *FunExpr
	closure *Environment
}

func (f *AnonFunction) Call(interpreter *Interpreter, arguments []any) any {
	localEnv := NewEnvironment(f.closure)

	for i := 0; i < len(f.expr.params); i++ {
		localEnv.Set(f.expr.params[i].lexeme, arguments[i])
	}

	body, ok := f.expr.body.(*Block)
	if !ok {
		panic("Unreachable.")
	}

	// TODO: Make this less ugly.
	// I don't want to have to force every Call implementation
	// to also return an error.
	err := interpreter.execBlock(body, localEnv)
	if ret, ok := err.(Return); ok {
		return ret.value
	}

	return err // Either nil or an error we want to handle
}

func (f *AnonFunction) Arity() int {
	return len(f.expr.params)
}

func (f *AnonFunction) String() string {
	return f.expr.String()
}
