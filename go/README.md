# Go implementation notes

## Pros
- Garbage collector

## Cons
- Does not allow default parameters for methods
- Does not allow method name overloading
- Is not really an OOP language so visibility is not really clear (found suggetions of having methods starting with a lower-case) 
- Allow mixing pass by value and reference making things slightly hard to understand
- No Pair/Tuple type
- Differnce between `:=` and `=` source of confusion and potential issues

## Workarounds
- Because of no default parameters and no method overloading I need two methods `Initialize` and `InitializeAndResertCursor`