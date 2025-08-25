package main

import "fmt"

type Stmt interface {
	isStmt()
	fmt.Stringer // Not sure if needed
}

type VarDecl struct {
	name Token
	expr Expr
}
func (*VarDecl) isStmt() {}
func (d VarDecl) String() string {
	return "var " + d.name.lexeme + " = " + d.expr.String() + ";"
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
