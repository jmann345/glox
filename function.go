package main

type Function struct {
	decl FunDecl
}

func (f Function) Call(interpreter *Interpreter, arguments []any) any {
	localEnv := NewEnvironment(interpreter.env)

	for i := 0; i < len(f.decl.params); i++ {
		localEnv.Set(f.decl.params[i].lexeme, arguments[i])
	}

	body, ok := f.decl.body.(*Block) // TODO: check if this should be *Block or Block
	if !ok { panic("Unreachable.") }

	// TODO: Make this less ugly. 
	// I don't want to have to force every Call implementation
	// to also return an error.
	err := interpreter.execBlock(*body, localEnv)
	if err != nil {
		return err
	}

	return nil
}
func (f Function) Arity() int {
	return len(f.decl.params)
}
func (f Function) String() string {
	return "<fn " + f.decl.name.lexeme + ">"
}
