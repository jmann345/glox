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

func bracketize(exprs ...Expr) string {
	var sb strings.Builder

	sb.WriteByte('[')
	for _, expr := range exprs {
		sb.WriteByte(',')
		sb.WriteByte(' ')
		sb.WriteString(expr.String())
	}
	sb.WriteByte(']')

	return sb.String()
}

type Assign struct {
	name  Token
	value Expr
}

func (Assign) isExpr() {}
func (a Assign) String() string {
	return parenthesize("assign "+a.name.lexeme, a.value)
}

type Binary struct {
	lhs Expr
	op  Token
	rhs Expr
}

func (Binary) isExpr() {}
func (b Binary) String() string {
	return parenthesize(b.op.lexeme, b.lhs, b.rhs)
}

type CallExpr struct {
	callee    Expr
	paren     Token
	arguments []Expr
}

func (CallExpr) isExpr() {}
func (c CallExpr) String() string {
	return parenthesize(
		c.paren.lexeme,
		append([]Expr{c.callee}, c.arguments...)...,
	)
}

type FunExpr struct {
	token  Token
	params []Token
	body   Stmt // NOTE: always a block statement
}

func (FunExpr) isExpr() {}
func (f FunExpr) String() string {
	var sb strings.Builder

	sb.WriteString("fun")

	sb.WriteByte('(')
	for i := range f.params {
		sb.WriteString(f.params[i].lexeme)
		if i < len(f.params)-1 {
			sb.WriteByte(',')
		}
	}
	sb.WriteByte(')')

	return sb.String()
}

type Get struct {
	object Expr
	name   Token
}

func (Get) isExpr() {}
func (g Get) String() string {
	return parenthesize(g.name.lexeme, g.object)
}

type Ternary struct {
	token       Token
	condition   Expr
	trueBranch  Expr
	falseBranch Expr
}

func (Ternary) isExpr() {}
func (t Ternary) String() string {
	return parenthesize(
		t.condition.String() + " ?", t.trueBranch, t.falseBranch,
	)
}

type Index struct {
	list    Expr
	bracket Token
	index   Expr
}

func (Index) isExpr() {}
func (i Index) String() string {
	return parenthesize("index", i.list, i.index)
}

type Grouping struct {
	expression Expr
}

func (Grouping) isExpr() {}
func (g Grouping) String() string {
	return parenthesize("group", g.expression)
}

type List struct {
	values []Expr
}

func (List) isExpr() {}
func (l List) String() string {
	return bracketize(l.values...)
}

type Literal struct {
	value any // float, string, boolean, or nil
}

func (Literal) isExpr() {}
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

type NoOpExpr struct{}

func (NoOpExpr) isExpr() {}
func (n NoOpExpr) String() string {
	return parenthesize("no-op")
}

type Set struct {
	object Expr
	name   Token
	value  Expr
}

func (Set) isExpr() {}
func (s Set) String() string {
	return parenthesize(s.name.lexeme, s.object, s.value)
}

type SetIndex struct {
	list    Expr
	bracket Token
	index   Expr
	value   Expr
}

func (SetIndex) isExpr() {}
func (s SetIndex) String() string {
	return parenthesize("set", s.list, s.index, s.value)
}

type AugSet struct {
	object Expr
	name   Token
	op 	   Token
	rhs    Expr
}

func (AugSet) isExpr() {}
func (a AugSet) String() string {
	return parenthesize(a.name.lexeme + a.op.lexeme, a.object, a.rhs)
}

type AugSetIndex struct {
	list    Expr
	bracket Token
	index   Expr
	op 	    Token
	rhs   	Expr
}

func (AugSetIndex) isExpr() {}
func (a AugSetIndex) String() string {
	return parenthesize("set" + a.op.lexeme, a.list, a.index, a.rhs)
}

type This struct {
	keyword Token
}

func (This) isExpr() {}
func (t This) String() string {
	return parenthesize(t.keyword.lexeme)
}

type Unary struct {
	op  Token
	rhs Expr
}

func (Unary) isExpr() {}
func (u Unary) String() string {
	return parenthesize(u.op.lexeme, u.rhs)
}

type Postfix struct {
	lhs Expr
	op  Token
}

func (Postfix) isExpr() {}
func (p Postfix) String() string {
	return parenthesize(p.op.lexeme, p.lhs)
}

type Variable struct {
	name Token
}

func (Variable) isExpr() {}
func (v Variable) String() string {
	return parenthesize(v.name.lexeme)
}
