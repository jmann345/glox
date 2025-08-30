package main

import "fmt"

func IsDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

func IsAlpha(c byte) bool {
	return (c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		c == '_'
}

func IsAlphaNumeric(c byte) bool {
	return IsAlpha(c) || IsDigit(c)
}

func Stringify(value any) string {
	switch v := value.(type) {
	case nil:
		return "nil"
	case bool, float64, string:
		return fmt.Sprintf("%v", v)
	case fmt.Stringer:
		return v.String()
	default:
		panic("Unreachable.")
	}
}

func SameType(lhs, rhs any) bool {
	switch lhs.(type) {
	case string:
		_, ok := rhs.(string)
		return ok
	case float64:
		_, ok := rhs.(float64)
		return ok
	case bool:
		_, ok := rhs.(bool)
		return ok
	case nil:
		return rhs == nil
	default:
		panic("Unreachable.")
	}
}
