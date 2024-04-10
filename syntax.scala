// op ::= `+` | `&&` | `<`
sealed trait Op
case object PlusOp extends Op
case object AndOp extends Op
case object LessThanOp extends Op

case class Variable(name: String)

// exp ::= INTEGER | `true` | `false` |
//         exp op exp
sealed trait Exp
case class IntegerLiteralExp(value: Int) extends Exp
case object TrueExp extends Exp
case object FalseExp extends Exp
case class VariableExp(theVar: Variable) extends Exp
case class BinopExp(left: Exp, op: Op, right: Exp) extends Exp

// type ::= `int` | `bool`
sealed trait Type
case object IntType extends Type
case object BoolType extends Type

sealed trait Stmt
case class VariableDeclarationStmt(theType: Type, theVar: Variable, exp: Exp) extends Stmt
case class PrintlnStmt(exp: Exp) extends Stmt
case class BlockStmt(stmts: Seq[Stmt]) extends Stmt

// program ::= exp
case class Program(stmts: Seq[Stmt])
