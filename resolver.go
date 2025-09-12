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

type VariableState byte

const (
	DECLARED VariableState = iota
	DEFINED
	USED
)

type VariableData struct {
	token Token
	state VariableState
}

type FunctionType byte

const (
	NONE FunctionType = iota
	FUNCTION
	INITIALIZER
	METHOD
	STATIC_METHOD
)

type ClassType byte

const ( // NONE and CLASS are taken :(
	NONE2 ClassType = iota
	KLASS
)

type Resolver struct {
	interpreter     *Interpreter
	scopes          Stack[map[string]*VariableData]
	currentFunction FunctionType
	currentClass    ClassType
	loopDepth       int
}

func NewResolver(interpreter *Interpreter) *Resolver {
	resolver := &Resolver{
		interpreter:     interpreter,
		scopes:          make(Stack[map[string]*VariableData], 0),
		currentFunction: NONE,
		currentClass:    NONE2,
		loopDepth:       0,
	}
	globals := resolver.beginScope()

	// Initialize our one built-in
	// If this language was larger, this is where I'd set up the prelude
	globals["clock"] = &VariableData{
		Token{IDENTIFIER, "clock", nil, -1}, DEFINED,
	}

	return resolver
}

func (r *Resolver) Resolve(stmts ...Stmt) error {
	for _, stmt := range stmts {
		if err := r.resolveStmt(stmt); err != nil {
			return err
		}
	}

	return nil
}

func (r *Resolver) beginScope() map[string]*VariableData {
	scope := make(map[string]*VariableData)
	r.scopes.Push(scope)

	return scope
}

func (r *Resolver) endScope(err *error) {
	if len(r.scopes) <= 1 { // never pop global scope
		return
	}

	scope := r.scopes.Pop()
	if *err != nil {
		return
	}

	for name, v := range scope {
		if v.state != USED {
			*err = ResolverError{v.token,
				"Declared and not used: " + name,
			}
			break
		}
	}
}

func (r *Resolver) declare(name Token) error {
	if r.scopes.Empty() {
		return nil
	}

	scope := r.scopes.Peek()
	if _, ok := scope[name.lexeme]; ok {
		return ResolverError{name,
			"Already a variable with this name in this scope.",
		}
	}

	scope[name.lexeme] = &VariableData{token: name, state: DECLARED}

	return nil
}

func (r *Resolver) define(name Token, state VariableState) {
	if r.scopes.Empty() {
		return
	}

	scope := r.scopes.Peek()
	scope[name.lexeme].state = state
}

func (r *Resolver) resolveLocal(expr Expr, name Token, markUsed bool) error {
	for i := len(r.scopes) - 1; i >= 0; i-- {
		if variable, ok := r.scopes[i][name.lexeme]; ok {
			r.interpreter.Resolve(expr, len(r.scopes)-i-1)

			if markUsed {
				variable.state = USED
			}

			return nil
		}
	}

	return ResolverError{name,
		"Undefined Variable '" + name.lexeme + "'.",
	}
}

func (r *Resolver) resolveStmt(stmt Stmt) error {
	switch s := stmt.(type) {
	case *Block:
		return r.resBlockStmt(s)
	case *BreakStmt:
		return r.resBreakStmt(s)
	case *ClassDecl:
		return r.resClassStmt(s)
	case *CycleStmt:
		return r.resCycleStmt(s)
	case *ExprStmt:
		return r.resExprStmt(s)
	case *ForStmt:
		return r.resForStmt(s)
	case *FunDecl:
		return r.resFunctionStmt(s, FUNCTION)
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

func (r *Resolver) resBlockStmt(stmt *Block) (err error) {
	r.beginScope()
	defer r.endScope(&err)

	for _, stmt := range stmt.stmts {
		if err = r.resolveStmt(stmt); err != nil {
			return
		}
	}

	return
}

func (r *Resolver) resBreakStmt(stmt *BreakStmt) error {
	if r.loopDepth == 0 {
		return ResolverError{stmt.keyword, Break{}.Error()}
	}

	return nil
}

func (r *Resolver) resClassStmt(stmt *ClassDecl) (err error) {
	enclosingClass := r.currentClass
	r.currentClass = KLASS

	defer func() {
		r.currentClass = enclosingClass
	}()

	if err = r.declare(stmt.name); err != nil {
		return
	}
	r.define(stmt.name, USED)

	closure := r.beginScope()
	closure["this"] = &VariableData{
		Token{THIS, "this", nil, -1}, USED, // Allow "this" to be unused
	}

	for _, meth := range stmt.methods {
		method, ok := meth.(*FunDecl)
		if !ok {
			panic("Unreachable.")
		}

		var declaration FunctionType
		if method.name.lexeme == "init" {
			declaration = INITIALIZER
		} else {
			declaration = METHOD
		}

		if err = r.resFunctionStmt(method, declaration); err != nil {
			r.endScope(&err)
			return
		}
	}

	// pop closure so static methods can't access 'this' or 'super'
	r.endScope(&err)

	r.beginScope() // static closure
	defer r.endScope(&err)

	for _, meth := range stmt.staticMethods {
		method, ok := meth.(*FunDecl)
		if !ok {
			panic("Unreachable.")
		}

		if err = r.resFunctionStmt(method, STATIC_METHOD); err != nil {
			return
		}
	}

	return
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

func (r *Resolver) resFunctionStmt(
	stmt *FunDecl, funTyp FunctionType,
) (err error) {
	if err = r.declare(stmt.name); err != nil {
		return
	}
	r.define(stmt.name, USED)

	enclosingFunction := r.currentFunction
	enclosingLoop := r.loopDepth

	r.currentFunction = funTyp
	r.loopDepth = 0

	r.beginScope()

	defer func() {
		r.currentFunction = enclosingFunction
		r.loopDepth = enclosingLoop
		r.endScope(&err)
	}()

	for _, param := range stmt.params {
		if err = r.declare(param); err != nil {
			return
		}
		r.define(param, DEFINED)
	}

	body, ok := stmt.body.(*Block)
	if !ok {
		panic("Unreachable.")
	}
	for _, s := range body.stmts {
		if err = r.resolveStmt(s); err != nil {
			return
		}
	}

	return
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
		return ResolverError{stmt.keyword,
			"Can't return from top-level code.",
		}
	}

	if stmt.value != nil {
		if r.currentFunction == INITIALIZER {
			return ResolverError{stmt.keyword,
				"Can't return a value from an initializer",
			}
		}

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

func (r *Resolver) resForStmt(stmt *ForStmt) (err error) {
	r.beginScope()
	defer r.endScope(&err)

	if err = r.resolveStmt(stmt.initializer); err != nil {
		return
	}

	if err = r.resolveExpr(stmt.condition); err != nil {
		return
	}

	if err = r.resolveExpr(stmt.increment); err != nil {
		return
	}

	r.loopDepth++
	defer func() { r.loopDepth-- }()

	if err = r.resolveStmt(stmt.body); err != nil {
		return
	}

	return
}

func (r *Resolver) resVarStmt(stmt *VarDecl) error {
	if err := r.declare(stmt.name); err != nil {
		return err
	}
	if stmt.initializer != nil {
		if err := r.resolveExpr(stmt.initializer); err != nil {
			for i := len(r.scopes) - 1; i >= 0; i-- {
				scope := r.scopes[i]
				// Un-declare variable if there was an error.
				// There's probably a better way of to do this.
				if _, ok := scope[stmt.name.lexeme]; ok {
					delete(scope, stmt.name.lexeme)
					break
				}
			}

			return err
		}
	}
	r.define(stmt.name, DEFINED)

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
	case *Get:
		return r.resGetExpr(e)
	case *Grouping:
		return r.resGroupingExpr(e)
	case *IfExpr:
		return r.resIfExpr(e)
	case *Literal, *NoOpExpr:
		return nil
	case *Postfix:
		return r.resPostfixExpr(e)
	case *Set:
		return r.resSetExpr(e)
	case *This:
		return r.resThisExpr(e)
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
		scope := r.scopes.Peek()
		if v, ok := scope[expr.name.lexeme]; ok && (v.state == DECLARED) {
			return ResolverError{expr.name,
				"Can't read local variable in its own initializer.",
			}
		}
	}

	return r.resolveLocal(expr, expr.name, true)
}

func (r *Resolver) resAssignExpr(expr *Assign) error {
	if err := r.resolveExpr(expr.value); err != nil {
		return err
	}

	return r.resolveLocal(expr, expr.name, false)
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

func (r *Resolver) resFunctionExpr(
	expr *FunExpr, funTyp FunctionType,
) (err error) {
	enclosingFunction := r.currentFunction
	enclosingLoop := r.loopDepth

	r.currentFunction = funTyp
	r.loopDepth = 0

	defer func() {
		r.currentFunction = enclosingFunction
		r.loopDepth = enclosingLoop
	}()

	r.beginScope()
	defer r.endScope(&err)

	for _, param := range expr.params {
		if err = r.declare(param); err != nil {
			return
		}
		r.define(param, DEFINED)
	}

	body, _ := expr.body.(*Block)
	for _, s := range body.stmts {
		if err = r.resolveStmt(s); err != nil {
			return
		}
	}

	return
}

func (r *Resolver) resGetExpr(expr *Get) error {
	return r.resolveExpr(expr.object)
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

func (r *Resolver) resSetExpr(expr *Set) error {
	if err := r.resolveExpr(expr.value); err != nil {
		return err
	}

	if err := r.resolveExpr(expr.object); err != nil {
		return err
	}

	return nil
}

func (r *Resolver) resThisExpr(expr *This) error {
	if r.currentClass == NONE2 {
		return ResolverError{expr.keyword,
			"Can't use 'this' outside of a class.",
		}
	}

	return r.resolveLocal(expr, expr.keyword, true)
}

func (r *Resolver) resUnaryExpr(expr *Unary) error {
	if err := r.resolveExpr(expr.rhs); err != nil {
		return err
	}

	return nil
}
