package main // TODO: Make this package interpreter

import "fmt" // TODO: Make Class and Instance extend Object

type Object interface {
	Get(name Token) (any, error)
	Set(name Token, value any) error
	fmt.Stringer
}
