package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
)

type LoxError struct {
	line    int
	where   string
	message string
}

func (e *LoxError) Error() string {
	return fmt.Sprintf(
		"[line %d] Error%s: %s",
		e.line, e.where, e.message,
	)
}

// TODO::Refactor to only initialize parts that require logic
// (but see if logic can be handled within pkl file)
// Also TODO : Take rgb values for all colors and use RGBToAttribute

func main() {
	// TODO: path should be var determined by user input

	// we say glox instead of jlox bc its golox not javalox!
	numArgs := len(os.Args)

	if numArgs > 2 {
		fmt.Println("Usage: glox <script>")
		os.Exit(64)
	} else if numArgs == 2 {
		err := runFile(os.Args[1])
		if err != nil {
			log.Fatal(err)
		}
	} else {
		err := runPrompt()
		if err != nil {
			// This might be a great place to handle syntax errors later!
			log.Fatal(err)
		}
	}
}

func runFile(path string) error {
	file, err := os.Open(path)
	if err != nil {
		return err
	}

	buff, err := io.ReadAll(file)
	if err != nil {
		return err
	}

	/* err := */
	interpreter := NewInterpreter()
	run(string(buff), interpreter) // may need to specify utf-8/acsii
	// NOTE: errs should be printed inside run()!
	// if err != nil { os.Exit(64) }

	return nil
}

func runPrompt() error {
	reader := bufio.NewReader(os.Stdin)
	interpreter := NewInterpreter()
	for {
		fmt.Print(":> ")

		line, err := reader.ReadString('\n')
		if err != nil {
			return err
		}

		if strings.TrimSpace(line) == "" {
			break
		}
		/* _ = */ run(line, interpreter) // error printed inside run
		// We ignore errors in runPrompt() bc it will just be printed, user can retry
	}

	return nil
}

// TODO: Add `context *CodeContext` parameter (maybe in shell mode only)
func run(source string, interpreter *Interpreter) {
	tokenizer := new(Tokenizer)
	tokenizer.Init(source)
	toks, errs := tokenizer.Tokenize()
	for _, err := range errs {
		fmt.Fprintln(os.Stderr, "Tokenizer:", err)
	}

	parser := Parser{toks, 0}
	for !parser.IsAtEnd() {
		stmt, err := parser.Parse()
		if err != nil {
			fmt.Fprintln(os.Stderr, "Parser:", err)
			continue
		}

		// TODO: multi-line interpretation
		vals, errs := interpreter.Interpret(stmt)
		val, err := vals[0], errs[0]
		if err != nil {
			fmt.Fprintln(os.Stderr, "Runtime:", err)
			// os.Exit(70) <-- Should only happen if running script tho
		} else {
			fmt.Println("expr: " + stmt.String())
			fmt.Println(val)
		}
	}
}

// TODO: |> Need triangle operator for something bro
