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

// Built-Ins:
type Clock struct{}

func (c Clock) Call(_ *Interpreter, _ []any) any {
	return float64(time.Now().UnixNano()) / 1e9
}

func (c Clock) Arity() int {
	return 0
}

func (c Clock) String() string {
	return "<native fn>"
}

type Len struct{}

func (l Len) Call(interpreter *Interpreter, arguments []any) any {
	arg, ok := arguments[0].([]any)
	if !ok {
		return RuntimeError{
			tok: Token{typ: IDENTIFIER, lexeme: "len"},
			msg: "Argument to len() must be a list.",
		}
	}

	return float64(len(arg))
}

func (l Len) Arity() int {
	return 1
}

func (l Len) String() string {
	return "<native fn>"
}

type Append struct{}

func (a Append) Call(interpreter *Interpreter, arguments []any) any {
	arg1, ok := arguments[0].([]any)
	if !ok {
		return RuntimeError{
			tok: Token{typ: IDENTIFIER, lexeme: "append"},
			msg: "First argument to append() must be a list.",
		}
	}

	arg2 := arguments[1]

	return append(arg1, arg2)
}

func (a Append) Arity() int {
	return 2
}

func (a Append) String() string {
	return "<native fn>"
}
