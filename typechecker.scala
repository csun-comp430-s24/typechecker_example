object Typechecker {
  def typecheckProgram(prog: Program): Unit = {
    typecheckStmts(prog.stmts, 0, Map())
  }

  def typecheckStmts(
    stmts: Seq[Stmt],
    scopeLevel: Int,
    env: Map[Variable, (Type, Int)]): Map[Variable, (Type, Int)] = {
    stmts.foldLeft(env)((accum, cur) =>
      typecheck(cur, scopeLevel, accum))
  }

  def typecheck(
    stmt: Stmt,
    scopeLevel: Int,
    env: Map[Variable, (Type, Int)]): Map[Variable, (Type, Int)] = {
    stmt match {
      case PrintlnStmt(exp) => {
        typeof(exp)
        env
      }
      // expectedType theVar = initializer;
      case VariableDeclarationStmt(expectedType, theVar, initializer) => {
        val receivedType = typeof(initializer, env)
        if (expectedType != receivedType) {
          throw TypeErrorException(
            "Expected type: " + expectedType +
              "; received type: " + receivedType)
        } else {
          env.get(theVar) match {
            case Some((_, `scopeLevel`)) => 
              throw TypeErrorException("Name in same scope: " + theVar)
            case _ =>
              env + (theVar -> (receivedType, scopeLevel))
          }
        }
      }
      case BlockStmt(stmts) => {
        // Map<Variable, Type> nestedEnv = new HashMap(env); // make a copy of env
        // for (int index = 0; index < stmts.length; index++) {
        //   nestedEnv = typecheck(stmts[index], nestedEnv);
        // }
        // return env;
        typecheckStmts(stmts, scopeLevel + 1, env)
        env
      }
    }
  }

  def typeof(exp: Exp, env: Map[Variable, (Type, Int)]): Type = {
    // if (exp instanceof IntegerLiteralExp) {
    //   return new IntType();
    // } else {
    val expType = exp match {
      case IntegerLiteralExp(_) => IntType
      case TrueExp | FalseExp => BoolType
      case VariableExp(theVar) => {
        // is the variable in scope?
        // if so, what's the type of the variable?
        env.get(theVar) match {
          case Some((typ, _)) => typ
          case None => throw TypeErrorException("Variable not in scope: " + theVar)
        }
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
    exp.typ = expType
    expType
  }
}

case class TypeErrorException(msg: String) extends Exception(msg)

