package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

type LoxError struct {
	line    int
	where   string
	message string
}

func (e LoxError) Error() string {
	return fmt.Sprintf(
		"[line %d] Error%s: %s",
		e.line, e.where, e.message,
	)
}

func main() {
	switch {
	case len(os.Args) == 1:
		if err := runPrompt(); err != nil {
			log.Fatal(err)
		}
	case len(os.Args) == 2:
		if err := runFile(os.Args[1], false); err != nil {
			log.Fatal(err)
		}
	case len(os.Args) == 3:
		if os.Args[1] == "--import" {
			if err := runFile(os.Args[2], true); err != nil {
				log.Fatal(err)
			}
		} else {
			fmt.Println("Usage: glox [--import <script>] | <script>")
			os.Exit(64)
		}
	default:
		fmt.Println("Usage: glox [--import <script>] | <script>")
		os.Exit(64)
	}
}

func runFile(path string, shell bool) error {
	file, err := os.Open(path)
	if err != nil {
		return err
	}

	buff, err := io.ReadAll(file)
	if err != nil {
		return err
	}

	interpreter := NewInterpreter()
	resolver := NewResolver(interpreter)

	run(string(buff), resolver)

	if shell {
		reader := bufio.NewReader(os.Stdin)
		for i := 1; ; i++ {
			fmt.Print("[" + strconv.Itoa(i) + "] ")

			line, err := reader.ReadString('\n')
			if err != nil {
				return err
			}

			if strings.TrimSpace(line) == "" {
				break
			}

			run(line, resolver)
		}
	}

	return nil
}

func runPrompt() error {
	reader := bufio.NewReader(os.Stdin)
	interpreter := NewInterpreter()
	resolver := NewResolver(interpreter)
	for i := 1; ; i++ {
		fmt.Print("[" + strconv.Itoa(i) + "] ")

		line, err := reader.ReadString('\n')
		if err != nil {
			return err
		}

		if strings.TrimSpace(line) == "" {
			break
		}

		run(line, resolver)
	}

	return nil
}

func run(source string, resolver *Resolver) {
	tokenizer := new(Tokenizer)
	tokenizer.Init(source)

	toks, errs := tokenizer.Tokenize()
	for _, err := range errs {
		fmt.Fprintln(os.Stderr, "Tokenizer:", err)
	}

	parser := Parser{toks, 0}
	stmts := []Stmt{}
	for !parser.IsAtEnd() {
		stmt, err := parser.Parse()
		if err != nil {
			fmt.Fprintln(os.Stderr, "Parser:", err)
			continue
		}

		stmts = append(stmts, stmt)
	}

	var err error
	for _, stmt := range stmts {
		if err = resolver.Resolve(stmt); err != nil {
			fmt.Fprintln(os.Stderr, err)
		}
	}

	if err == nil {
		interpreter := resolver.interpreter
		for _, stmt := range stmts {
			if err = interpreter.Interpret(stmt); err != nil {
				fmt.Fprintln(os.Stderr, err)
			}
		}
	}
}
