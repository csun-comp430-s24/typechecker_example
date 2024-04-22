# Typechecker Example #

```
op ::= `+` | `&&` | `<`
exp ::= INTEGER | `true` | `false` |
        VARIABLE | exp op exp |
		FUNCTION_NAME `(` exp* `)`
type ::= `int` | `bool`
stmt ::= type VARIABLE `=` exp `;` |
         `println` `(` exp `)` `;` |
		 `{` stmt* `}` |
		 `return` exp `;`
formal_arg ::= type VARIABLE
func ::= type FUNCTION_NAME `(` [formal_arg (`,` formal_arg)*] `) stmt
program ::= func*
```
