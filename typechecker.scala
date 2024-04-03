object Typechecker {
  def typeof(exp: Exp): Type = {
    // if (exp instanceof IntegerLiteralExp) {
    //   return new IntType();
    // } else {
    exp match {
      case IntegerLiteralExp(_) => IntType
      case TrueExp | FalseExp => BoolType
      case BinopExp(left, op, right) => {
        val leftType: Type = typeof(left)
        val rightType: Type = typeof(right)
        // if (leftType instanceof IntType &&
        //     op instanceof LessThanOp &&
        //     rightType instanceof IntType) {
        //    return new BoolType();
        // }
        (leftType, op, rightType) match {
          case (IntType, LessThanOp, IntType) => BoolType
          // case (IntType, LessThanOp, BoolType) => {
          //   right.typeShouldBe = IntType
          case (BoolType, AndOp, BoolType) => BoolType
          case (IntType, PlusOp, IntType) => IntType
        }
      }
    }
  }
}
