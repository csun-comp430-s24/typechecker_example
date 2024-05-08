# Typechecker Example #

```
struct Foo {
  int x;
  bool y;
}

Foo z = Foo { x: 5, y: true };
println(x.y.z)
```

```
op ::= `+` | `&&` | `<`
exp ::= INTEGER | `true` | `false` |
        VARIABLE | exp op exp |
		FUNCTION_NAME `(` exp* `)` |
		STRUCT_NAME `{` (VARIABLE `:` exp)* `}` |
		exp `.` VARIABLE
type ::= `int` | `bool` | STRUCT_NAME
stmt ::= type VARIABLE `=` exp `;` |
         `println` `(` exp `)` `;` |
		 `{` stmt* `}` |
		 `return` exp `;`
formal_arg ::= type VARIABLE
func ::= type FUNCTION_NAME `(` [formal_arg (`,` formal_arg)*] `) stmt
struct_arg ::= formal_arg `;`
struct ::= `struct` STRUCT_NAME `{` struct_arg* `}` 
program ::= struct* func*
```
