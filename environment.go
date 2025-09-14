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

func (e *Environment) GetAt(distance int, key string) any {
	for range distance {
		e = e.enclosing
		if e == nil { panic("GetAt: bad distance for " + key) }
	}

	return e.values[key]
}

func (e *Environment) Set(key string, value any) {
	e.values[key] = value
}

func (e *Environment) SetAt(distance int, key string, value any) {
	for range distance {
		e = e.enclosing
		if e == nil { panic("SetAt: bad distance for " + key) }
	}

	e.values[key] = value
}

func (e *Environment) SetInScope(key string, value any) {
	for _, ok := e.values[key]; !ok; _, ok = e.values[key] {
		e = e.enclosing
	}

	e.values[key] = value
}
