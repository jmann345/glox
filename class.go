package main

type Class struct {
	Name    string
	Methods map[string]*Function
}

func (c Class) Call(interpreter *Interpreter, arguments []any) any {
	instance := new(Object)
	instance.Init(c)

	if initializer, ok := c.Methods["init"]; ok {
		initializer.Bind(instance).Call(interpreter, arguments)
	}

	return instance
}

func (c Class) Arity() int {
	if initializer, ok := c.Methods["init"]; ok {
		return initializer.Arity()
	}

	return 0
}

func (c Class) String() string {
	return c.Name
}
