package main

// Instances are objects but not classes
type Instance struct {
	class  *Class
	fields map[string]any
}

func (i *Instance) Get(name Token) (any, error) {
	if attr, ok := i.fields[name.lexeme]; ok {
		return attr, nil
	}

	if meth, ok := i.class.Methods[name.lexeme]; ok {
		return meth.Bind(i), nil
	}

	if meth, ok := i.class.StaticMethods[name.lexeme]; ok {
		return meth, nil
	}

	return nil, RuntimeError{
		name, "Undefined property '" + name.lexeme + "'.",
	}
}

func (i *Instance) Set(name Token, value any) error {
	i.fields[name.lexeme] = value

	return nil
}

func (i *Instance) String() string {
	return i.class.Name + " instance"
}
