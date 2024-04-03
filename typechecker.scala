object Typechecker {
  def typeof(exp: Exp, env: Map[Variable, Type]): Type = {
    // if (exp instanceof IntegerLiteralExp) {
    //   return new IntType();
    // } else {
    exp match {
      case IntegerLiteralExp(_) => IntType
      case TrueExp | FalseExp => BoolType
      case VariableExp(theVar) => {
        // is the variable in scope?
        // if so, what's the type of the variable?
      }
      case BinopExp(left, op, right) => {
        val leftType: Type = typeof(left)
        val rightType: Type = typeof(right)
        // if (leftType instanceof IntType &&
        //     op instanceof LessThanOp &&
        //     rightType instanceof IntType) {
        //    return new BoolType();
        // }
        val tup = (leftType, op, rightType)
        tup match {
          case (IntType, LessThanOp, IntType) => BoolType
          // case (IntType, LessThanOp, BoolType) => {
          //   right.typeShouldBe = IntType
          case (BoolType, AndOp, BoolType) => BoolType
          case (IntType, PlusOp, IntType) => IntType
          case _ => throw TypeErrorException("Bad types: " + tup)
        }
      }
    }
  }
}

case class TypeErrorException(msg: String) extends Exception(msg)

