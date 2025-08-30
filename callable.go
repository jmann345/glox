package main

import (
	"fmt"
	"time"
)

type Callable interface {
	Call(interpreter *Interpreter, arguments []any) any
	Arity() int
	fmt.Stringer
}

// Built-In Example:
type Clock struct{}

func (c Clock) Call(interpreter *Interpreter, arguments []any) any {
	return float64(time.Now().UnixNano()) / 1e9
}
func (c Clock) Arity() int {
	return 0
}
func (c Clock) String() string {
	return "<native fn>"
}
