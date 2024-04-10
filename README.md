# Typechecker Example #

```
op ::= `+` | `&&` | `<`
exp ::= INTEGER | `true` | `false` |
        VARIABLE | exp op exp
type ::= `int` | `bool`
stmt ::= type VARIABLE `=` exp `;` |
         `println` `(` exp `)` `;` |
		 `{` stmt* `}`
program ::= stmt*
```
