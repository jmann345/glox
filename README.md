# glox

glox started as a Go-based interpreter for Lox inspired by @munificent, but it has evolved into a similar yet noticeably distinct language. I highly reccomend checking out the [book](https://craftinginterpreters.com/) to learn more about Lox and how my implementation is different!

-------------------------------------------------------------------------------

## Overview

glox supports first-class functions, lists, classes (with static and instance methods), and extended flow control. It runs on a tree-walk interpreter written entirely in Go, using no external dependencies. The project demonstrates how a small language can grow beyond its origins into something unique.

-------------------------------------------------------------------------------

## Key Differences from Lox

1. "#" for single-line comments and "#[ ... ]#" for block comments  
2. "not" replaces "!" for logical negation  
3. If and loop expressions require braces around the body, but do not require parentheses around the conditions
4. Only "false" is falsey
5. Supports prefix and postfix ++ and --  
6. Supports "+=", "-=", "*=", and "/=" operators  
7. Comma operator evaluates left to right and yields the last value  
8. Supports dynamic lists
9. Supports "cycle" (continue) and "break" statements
10. Classes support static methods via "class" keyword inside the class body  
11. Resolver enforces strong semantic checks for early error detection  

-------------------------------------------------------------------------------

## Language Features

### Variables
```
var x = 10;
x += 2;
print x;  # 12
```

### Comments
```
# Single line comment
#[
This is a block comment.
It can span multiple lines.
]#
```

### Control Flow
```
if x > 10 {
  print "big";
} else {
  print "small";
}

while x < 20 {
  x++;
}

for var i = 0; i < 5; i++ {
  if i == 2 { cycle; }
  print i;
}
```

### Lists
```
var xs = [1, 2, 3];
append(xs, 4);
print len(xs);  # 4
print xs[2];    # 3
xs[0] = 99;
```
### Functions
```
fun greet(name) {
  print "Hello, " + name;
}

# Glox also supports anonymous functions:
var add = fun(a, b) {
  return a + b;
};

print add(2, 3);  # 5
```

### Classes
```
class Vec {
    init() {
        this.lst = [];
    }

    class from(lst) {
        var vec = Vec();

        for var i = 0; i < len(lst); i++ {
            vec.push(lst[i]);
        }

        return vec;
    }

    get(i) {
        return this.lst[i];
    }

    len() {
        return len(this.lst);
    }

    empty() {
        return this.len() == 0;
    }

    toList() {
        return this.lst;
    }

    map(fn) {
        var out = Vec();

        for var i = 0; i < this.len(); i++ {
            var x = this.get(i);
            out.push(fn(x));
        }

        return out;
    }

    filter(fn) {
        var out = Vec();
        for var i = 0; i < this.len(); i++ {
            var x = this.get(i);
            if fn(x) {
                out.push(x);
            }
        }
        return out;
    }

    pprint() {
        print this.lst;
    }

    # Mutation methods
    set(i, x) {
        this.lst[i] = x;
    }

    push(x) {
        this.lst = append(this.lst, x);
    }

    append(xs) {
        if type(xs) == "list" {
            for var i = 0; i < len(xs); i++ {
                this.push(xs[i]);
            }
        } else if type(xs) == "Vec" {
            for var i = 0; i < xs.len(); i++ {
                this.push(xs.get(i));
            }
        } else {
            print "Invalid type: " + type(xs);
            print "Did you mean to call push()?";
        }
    }
}

var vec = Vec.from([1, 4, 9, 16]);

vec.map(fun(x) { return x * x; })
   .filter(fun(x) { return x > 5 and x < 1024; })
   .pprint(); # prints [16, 81, 256]
```

-------------------------------------------------------------------------------

## Builtins

clock() - returns seconds since Unix epoch as a float  

len(list) - returns the number of elements in a list  

append(list, val) - returns a new list with val appended at the end

type(obj) - returns the type of an object in string format 

-------------------------------------------------------------------------------

## CLI Usage

Build:
```
git clone https://github.com/jmann345/glox
cd glox
go build -o glox .
```
Run a script:
```
./glox path/to/script.lox
```

Start the REPL:
```
./glox
[1] print "hello world";
hello world
```
