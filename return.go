package main

type Return struct {
	value any
}

// We impl error so we can handle return statements properly
// But it's not actually an error
func (r Return) Error() string {
	return "THIS SHOULD NEVER BE CALLED!"
}
