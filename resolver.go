package main

import (
	"fmt"
)

type ResolverError struct {
	tok Token
	msg string
}

func (e ResolverError) Error() string {
	return fmt.Sprintf("[line %d] Resolution Error at '%s': %s",
		e.tok.line, e.tok.lexeme, e.msg)
}

type FunctionType byte

const (
	NONE FunctionType = iota
	FUNCTION
)

type Resolver struct {
	interpreter     *Interpreter
	scopes          Stack[map[string]bool]
	currentFunction FunctionType
	loopDepth       int
}

func NewResolver(interpreter *Interpreter) *Resolver {
	return &Resolver{
		interpreter:     interpreter,
		scopes:          make(Stack[map[string]bool], 0),
		currentFunction: NONE,
		loopDepth:       0,
	}
}

func (r *Resolver) Resolve(stmts ...Stmt) error {
	for _, stmt := range stmts {
		if err := r.resolveStmt(stmt); err != nil {
			return err
		}
	}

	return nil
}

func (r *Resolver) beginScope() {
	r.scopes.Push(make(map[string]bool))
}

func (r *Resolver) endScope() {
	r.scopes.Pop()
}

func (r *Resolver) declare(name Token) error {
	if r.scopes.Empty() {
		return nil
	}

	scope := r.scopes.Peek()
	if _, ok := scope[name.lexeme]; ok {
		return ResolverError{name,
			"Already a variable with this name in this scope."}
	}

	scope[name.lexeme] = false
	return nil
}

func (r *Resolver) define(name Token) {
	if r.scopes.Empty() {
		return
	}

	scope := r.scopes.Peek()
	scope[name.lexeme] = true
}

func (r *Resolver) resolveLocal(expr Expr, name Token) {
	for i := len(r.scopes) - 1; i >= 0; i-- {
		if _, ok := r.scopes[i][name.lexeme]; ok {
			r.interpreter.Resolve(expr, len(r.scopes)-1-i)
			return
		}
	}
}

func (r *Resolver) resolveStmt(stmt Stmt) error {
	switch s := stmt.(type) {
	case *Block:
		return r.resBlockStmt(s)
	case *BreakStmt:
		return r.resBreakStmt(s)
	case *CycleStmt:
		return r.resCycleStmt(s)
	case *ExprStmt:
		return r.resExprStmt(s)
	case *ForStmt:
		return r.resForStmt(s)
	case *FunDecl:
		return r.resFunctionStmt(s, FUNCTION) // Not sure if this is right
	case *IfStmt:
		return r.resIfStmt(s)
	case *NoOpStmt:
		return nil
	case *PrintStmt:
		return r.resPrintStmt(s)
	case *ReturnStmt:
		return r.resReturnStmt(s)
	case *WhileStmt:
		return r.resWhileStmt(s)
	case *VarDecl:
		return r.resVarStmt(s)
	default:
		panic(fmt.Sprintf("Unresolved Statement type: %T", s))
	}
}

func (r *Resolver) resBlockStmt(stmt *Block) error {
	r.beginScope()
	defer r.endScope()

	for _, stmt := range stmt.stmts {
		if err := r.resolveStmt(stmt); err != nil {
			return err
		}
	}

	return nil
}

func (r *Resolver) resBreakStmt(stmt *BreakStmt) error {
	if r.loopDepth == 0 {
		return ResolverError{stmt.keyword, Break{}.Error()}
	}

	return nil
}

func (r *Resolver) resCycleStmt(stmt *CycleStmt) error {
	if r.loopDepth == 0 {
		return ResolverError{stmt.keyword, Cycle{}.Error()}
	}

	return nil
}

func (r *Resolver) resExprStmt(stmt *ExprStmt) error {
	return r.resolveExpr(stmt.expr)
}

func (r *Resolver) resFunctionStmt(stmt *FunDecl, funTyp FunctionType) error {
	if err := r.declare(stmt.name); err != nil {
		return err
	}
	r.define(stmt.name) // define to enable recursion

	enclosingFunction := r.currentFunction
	enclosingLoop := r.loopDepth

	r.currentFunction = funTyp
	r.loopDepth = 0

	defer func() {
		r.currentFunction = enclosingFunction
		r.loopDepth = enclosingLoop
	}()

	r.beginScope()
	defer r.endScope()

	for _, param := range stmt.params {
		if err := r.declare(param); err != nil {
			return err
		}
		r.define(param)
	}

	body, ok := stmt.body.(*Block)
	if !ok {
		panic("Unreachable.")
	}
	for _, s := range body.stmts {
		if err := r.resolveStmt(s); err != nil {
			return err
		}
	}

	return nil
}

func (r *Resolver) resIfStmt(stmt *IfStmt) error {
	if err := r.resolveExpr(stmt.condition); err != nil {
		return err
	}

	if err := r.resolveStmt(stmt.thenBranch); err != nil {
		return err
	}

	if stmt.elseBranch != nil {
		if err := r.resolveStmt(stmt.elseBranch); err != nil {
			return err
		}
	}

	return nil
}

func (r *Resolver) resPrintStmt(stmt *PrintStmt) error {
	return r.resolveExpr(stmt.expr)
}

func (r *Resolver) resReturnStmt(stmt *ReturnStmt) error {
	if r.currentFunction == NONE {
		return ResolverError{stmt.keyword, "Can't return from top-level code."}
	}

	if stmt.value != nil {
		return r.resolveExpr(stmt.value)
	}

	return nil
}

func (r *Resolver) resWhileStmt(stmt *WhileStmt) error {
	if err := r.resolveExpr(stmt.condition); err != nil {
		return err
	}

	r.loopDepth++
	defer func() { r.loopDepth-- }()

	return r.resolveStmt(stmt.body)
}

func (r *Resolver) resForStmt(stmt *ForStmt) error {
	r.beginScope()
	defer r.endScope()

	if err := r.resolveStmt(stmt.initializer); err != nil {
		return err
	}

	if err := r.resolveExpr(stmt.condition); err != nil {
		return err
	}

	if err := r.resolveExpr(stmt.increment); err != nil {
		return err
	}

	r.loopDepth++
	defer func() { r.loopDepth-- }()

	if err := r.resolveStmt(stmt.body); err != nil {
		return err
	}

	return nil
}

func (r *Resolver) resVarStmt(stmt *VarDecl) error {
	if err := r.declare(stmt.name); err != nil {
		return err
	}
	if stmt.initializer != nil {
		if err := r.resolveExpr(stmt.initializer); err != nil {
			return err
		}
	}
	r.define(stmt.name)

	return nil
}

func (r *Resolver) resolveExpr(expr Expr) error {
	switch e := expr.(type) {
	case *Assign:
		return r.resAssignExpr(e)
	case *Binary:
		return r.resBinaryExpr(e)
	case *CallExpr:
		return r.resCallExpr(e)
	case *FunExpr:
		return r.resFunctionExpr(e, FUNCTION) // not sure if NONE is right
	case *Grouping:
		return r.resGroupingExpr(e)
	case *IfExpr:
		return r.resIfExpr(e)
	case *Literal, *NoOpExpr:
		return nil
	case *Postfix:
		return r.resPostfixExpr(e)
	case *Unary:
		return r.resUnaryExpr(e)
	case *Variable:
		return r.resVarExpr(e)
	default:
		panic(fmt.Sprintf("Unresolved Expression type: %T", e))
	}
}

func (r *Resolver) resVarExpr(expr *Variable) error {
	if !r.scopes.Empty() {
		if v, ok := r.scopes.Peek()[expr.name.lexeme]; ok && !v {
			return ResolverError{expr.name,
				"Can't read local variable in its own initializer."}
		}
	}

	r.resolveLocal(expr, expr.name)

	return nil
}

func (r *Resolver) resAssignExpr(expr *Assign) error {
	if err := r.resolveExpr(expr.value); err != nil {
		return err
	}
	r.resolveLocal(expr, expr.name)

	return nil
}

func (r *Resolver) resBinaryExpr(expr *Binary) error {
	if err := r.resolveExpr(expr.lhs); err != nil {
		return err
	}

	if err := r.resolveExpr(expr.rhs); err != nil {
		return err
	}

	return nil
}

func (r *Resolver) resCallExpr(expr *CallExpr) error {
	if err := r.resolveExpr(expr.callee); err != nil {
		return err
	}

	for _, arg := range expr.arguments {
		if err := r.resolveExpr(arg); err != nil {
			return err
		}
	}

	return nil
}

func (r *Resolver) resFunctionExpr(expr *FunExpr, funTyp FunctionType) error {
	enclosingFunction := r.currentFunction
	enclosingLoop := r.loopDepth

	r.currentFunction = funTyp
	r.loopDepth = 0

	defer func() {
		r.currentFunction = enclosingFunction
		r.loopDepth = enclosingLoop
	}()

	r.beginScope()
	defer r.endScope()

	for _, param := range expr.params {
		if err := r.declare(param); err != nil {
			return err
		}
		r.define(param)
	}

	body, ok := expr.body.(*Block)
	if !ok {
		panic("Unreachable.")
	}
	for _, s := range body.stmts {
		if err := r.resolveStmt(s); err != nil {
			return err
		}
	}

	return nil
}

func (r *Resolver) resGroupingExpr(expr *Grouping) error {
	return r.resolveExpr(expr.expression)
}

func (r *Resolver) resIfExpr(expr *IfExpr) error {
	if err := r.resolveExpr(expr.condition); err != nil {
		return err
	}

	if err := r.resolveExpr(expr.thenBranch); err != nil {
		return err
	}

	if expr.elseBranch != nil {
		if err := r.resolveExpr(expr.elseBranch); err != nil {
			return err
		}
	}

	return nil
}

func (r *Resolver) resPostfixExpr(expr *Postfix) error {
	return r.resolveExpr(expr.lhs)
}

func (r *Resolver) resUnaryExpr(expr *Unary) error {
	if err := r.resolveExpr(expr.rhs); err != nil {
		return err
	}

	return nil
}
