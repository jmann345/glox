package main // TODO: Make this package interpreter

type Object struct {
	class  Class
	fields map[string]any
}

func (o *Object) Init(class Class) {
	o.class = class
	o.fields = make(map[string]any)
}

func (o Object) String() string {
	return o.class.Name + " instance"
}

func (o *Object) Get(name Token) (any, error) {
	if attr, ok := o.fields[name.lexeme]; ok {
		return attr, nil
	}

	if meth, ok := o.class.Methods[name.lexeme]; ok {
		return meth.Bind(o), nil
	}

	return nil, RuntimeError{
		name, "Undefined property '" + name.lexeme + "'.",
	}
}

func (o *Object) Set(name Token, value any) {
	o.fields[name.lexeme] = value
}
