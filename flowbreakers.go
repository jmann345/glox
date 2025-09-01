package main

type Break struct{}

func (Break) Error() string {
	return "'break' statement not in loop statement"
}

type Cycle struct{}

func (Cycle) Error() string {
	return "'cycle' statement not in loop statement"
}

type Return struct {
	value any
}

func (Return) Error() string {
	return "'return' can be used only within a function"
}
