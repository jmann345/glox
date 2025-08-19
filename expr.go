package main

import (
	"fmt"
	"strconv"
	"strings"
)

type Expr interface {
	isExpr()
	fmt.Stringer
}

type Assign struct {
	name  Token
	value Expr
}

func (*Assign) isExpr() {}
func (a Assign) String() string {
	return parenthesize("assign "+a.name.lexeme, a.value)
}

type Binary struct {
	lhs Expr
	op  Token
	rhs Expr
}

func (*Binary) isExpr() {}
func (b Binary) String() string {
	return parenthesize(b.op.lexeme, b.lhs, b.rhs)
}

type Grouping struct {
	expression Expr
}

func (*Grouping) isExpr() {}
func (g Grouping) String() string {
	return parenthesize("group", g.expression)
}

type Literal struct {
	value any // float, string, boolean, or nil
}

func (*Literal) isExpr() {}
func (l Literal) String() string {
	switch v := l.value.(type) {
	case string:
		return v
	case float64:
		return strconv.FormatFloat(v, 'f', -1, 64)
	case int:
		return strconv.FormatFloat(float64(v), 'f', -1, 64)
	case bool:
		if v {
			return "true"
		} else {
			return "false"
		}
	case nil:
		return "nil"
	default:
		msg := fmt.Sprintf("Incompatible type: %T", v)
		panic(msg)
	}
}

type Unary struct {
	op  Token
	rhs Expr
}

func (*Unary) isExpr() {}
func (u Unary) String() string {
	return parenthesize(u.op.lexeme, u.rhs)
}

func parenthesize(name string, exprs ...Expr) string {
	var sb strings.Builder
	sb.WriteByte('(')
	sb.WriteString(name)
	for _, expr := range exprs {
		sb.WriteByte(' ')
		sb.WriteString(expr.String())
	}
	sb.WriteByte(')')

	return sb.String()
}
