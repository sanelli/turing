# Go implementation notes
## Cons
- `go` does not allow default parameters for methods
- `go` does not allow method name overloading

## Workarounds
- Because of no default parameters and no method overloading I need two methods `Initialize` and `InitializeAndResertCursor`