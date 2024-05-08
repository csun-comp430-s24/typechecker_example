// op ::= `+` | `&&` | `<`
sealed trait Op
case object PlusOp extends Op
case object AndOp extends Op
case object LessThanOp extends Op

case class Variable(name: String)
case class FunctionName(name: String)
case class StructName(name: String)

// exp ::= INTEGER | `true` | `false` |
//         exp op exp
sealed trait Exp
case class IntegerLiteralExp(value: Int) extends Exp
case object TrueExp extends Exp
case object FalseExp extends Exp
case class VariableExp(theVar: Variable) extends Exp
case class BinopExp(left: Exp, op: Op, right: Exp) extends Exp
case class CallExp(name: FunctionName, params: Seq[Exp]) extends Exp
case class MakeStructExp(name: StructName, fields: Seq[(Variable, Exp)]) extends Exp
case class DotExp(exp: Exp, variable: Variable) extends Exp

// type ::= `int` | `bool` | STRUCT_NAME
sealed trait Type
case object IntType extends Type
case object BoolType extends Type
case class StructType(name: StructName) extends Type

sealed trait Stmt
case class VariableDeclarationStmt(theType: Type, theVar: Variable, exp: Exp) extends Stmt
case class PrintlnStmt(exp: Exp) extends Stmt
case class BlockStmt(stmts: Seq[Stmt]) extends Stmt
case class ReturnStmt(exp: Exp) extends Stmt

case class FormalArg(typ: Type, theVar: Variable)

case class Func(
  returnType: Type,
  name: FunctionName,
  args: Seq[FormalArg],
  body: Stmt)

case class StructDef(name: StructName, args: Seq[FormalArg])

// program ::= func*
case class Program(structs: Seq[StructDef], funcs: Seq[Func])

