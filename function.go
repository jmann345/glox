package main

type Function struct {
	decl    *FunDecl
	closure *Environment
}

func (f *Function) Call(interpreter *Interpreter, arguments []any) any {
	localEnv := NewEnvironment(f.closure)

	for i := 0; i < len(f.decl.params); i++ {
		localEnv.Set(f.decl.params[i].lexeme, arguments[i])
	}

	localEnv.Set(f.decl.name.lexeme, f) // Define function in its own scope so recursion works properly

	body, ok := f.decl.body.(*Block)
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
