package main

// Classes implement Callable and Object
type Class struct {
	Name          string
	Methods       map[string]*Function
	StaticMethods map[string]*Function
}

func (c *Class) Get(name Token) (any, error) {
	if meth, ok := c.StaticMethods[name.lexeme]; ok {
		return meth, nil
	}

	return nil, RuntimeError{
		name, "Undefined class method '" + name.lexeme + "'.",
	}
}

func (c *Class) Set(name Token, value any) error {
	if meth, ok := value.(*Function); ok {
		c.StaticMethods[name.lexeme] = meth
		return nil
	}

	// For now, I won't allow for class variables (i.e. cls.foo = "bar")
	return RuntimeError{
		name, "Can only assign non-anonymous methods to classes.",
	}
}

func (c *Class) Call(interpreter *Interpreter, arguments []any) any {
	instance := &Instance{c, make(map[string]any)}

	if initializer, ok := c.Methods["init"]; ok {
		ret := initializer.Bind(instance).Call(interpreter, arguments)
		if err, ok := ret.(error); ok {
			return err
		}
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
