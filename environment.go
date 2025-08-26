package main

type Environment struct {
	enclosing *Environment
	values    map[string]any
}

func NewEnvironment(enclosing *Environment) *Environment {
	return &Environment{enclosing, make(map[string]any)}
}

func (e *Environment) Get(key string) (any, bool) {
	val, ok := e.values[key]
	if ok {
		return val, true
	}

	if e.enclosing != nil {
		return e.enclosing.Get(key)
	}

	return nil, false
}

func (e *Environment) Set(key string, value any) {
	e.values[key] = value
}

func (e *Environment) SetInScope(key string, value any) {
	for _, ok := e.values[key]; !ok; _, ok = e.values[key] {
		e = e.enclosing
		if e == nil {
			panic("Unreachable.")
		}

		_, ok = e.values[key]
	}

	e.values[key] = value
}
