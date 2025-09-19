package main // TODO: Make this package interpreter

import "fmt"

type Object interface {
	Get(name Token) (any, error)
	Set(name Token, value any) error
	fmt.Stringer
}
