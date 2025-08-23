package main

type Environment struct {
	enclosing *Environment
	values    map[string]any
}

func NewEnvironment(enclosing *Environment) *Environment {
	return &Environment{enclosing, make(map[string]any)}
}

func (e *Environment) get(key string) (any, bool) {
	val, ok := e.values[key]
	if ok {
		return val, true
	}

	if e.enclosing != nil {
		return e.enclosing.get(key)
	}

	return nil, false
}

func (e *Environment) set(key string, value any) {
	e.values[key] = value
}
