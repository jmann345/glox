package main

import (
	"fmt"
	"strings"
)

type Stmt interface {
	isStmt()
	fmt.Stringer // TODO: Standardize implementations or delete the stringer requirement
}

type NoOpStmt struct{}

func (*NoOpStmt) isStmt() {}
func (n NoOpStmt) String() string {
	return "no-op"
}

type VarDecl struct {
	name Token
	expr Expr
}

func (*VarDecl) isStmt() {}
func (d VarDecl) String() string {
	return "var " + d.name.lexeme + " = " + d.expr.String() + ";"
}

type FunDecl struct {
	name   Token
	params []Token
	body   Stmt // NOTE: book uses []Stmt, but `body` is always a Block stmt
}

func (*FunDecl) isStmt() {}
func (f FunDecl) String() string {
	return f.name.lexeme
}

type ExprStmt struct {
	expr Expr
}

func (*ExprStmt) isStmt() {}
func (e ExprStmt) String() string {
	return e.expr.String() + ";"
}

type PrintStmt struct {
	expr Expr
}

func (*PrintStmt) isStmt() {}
func (p PrintStmt) String() string {
	return "print " + p.expr.String() + ";"
}

type IfStmt struct {
	token      Token
	condition  Expr
	thenBranch Stmt
	elseBranch Stmt
}

func (*IfStmt) isStmt() {}
func (i IfStmt) String() string {
	var sb strings.Builder

	sb.WriteString("if ")
	sb.WriteString(i.condition.String())
	sb.WriteByte('{')
	sb.WriteString(i.thenBranch.String())
	sb.WriteByte('}')

	if i.elseBranch != nil {
		// Technically, this have different logic for "else" and "else if"
		// branches. Braces won't be shown correctly on else branches.
		// But I won't fix it for now
		sb.WriteString("else ")
		sb.WriteString(i.elseBranch.String())
	}

	return sb.String()
}

type WhileStmt struct {
	token     Token
	condition Expr
	body      Stmt
}

func (*WhileStmt) isStmt() {}
func (w WhileStmt) String() string {
	var sb strings.Builder

	sb.WriteString("while ")
	sb.WriteString(w.condition.String())
	sb.WriteByte('{')
	sb.WriteByte('\n')
	sb.WriteString(w.body.String())
	sb.WriteByte('}')

	return sb.String()
}

type ForStmt struct {
	token       Token
	initializer Stmt
	condition   Expr
	increment   Expr // increment clause
	body        Stmt
}

func (*ForStmt) isStmt() {}
func (w ForStmt) String() string {
	var sb strings.Builder

	sb.WriteString("for ")
	sb.WriteString(fmt.Sprintf("%s; %s; %s", w.initializer, w.condition, w.increment))
	sb.WriteByte('{')
	sb.WriteByte('\n')
	sb.WriteString(w.body.String())
	sb.WriteByte('}')

	return sb.String()
}

type Block struct {
	stmts []Stmt
}

func (*Block) isStmt() {}
func (b Block) String() string {
	s := ""
	for _, stmt := range b.stmts {
		s += (stmt.String() + "\n")
	}
	return s
}
