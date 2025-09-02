package main

type Stack[T any] []T

func (s *Stack[T]) Push(v T) {
	*s = append(*s, v)
}

func (s *Stack[T]) Peek() T {
	top := (*s)[len(*s)-1]
	return top
}

func (s *Stack[T]) Pop() T {
	top := s.Peek()
	*s = (*s)[0 : len(*s)-1]

	return top
}

func (s *Stack[T]) Empty() bool {
	return len(*s) == 0
}
