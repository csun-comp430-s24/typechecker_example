sealed trait ReturnAnalysis
case object NoReturn extends ReturnAnalysis
case object MaybeReturns extends ReturnAnalysis
case object DefinitelyReturns extends ReturnAnalysis

object Typechecker {
  def makeEnv(args: Seq[FormalArg]): Map[Variable, Type] = {
    args.foldLeft(Map[Variable, (Type, Int)]())((accum, cur) => {
      if (accum.contains(cur.theVar)) {
        throw TypeErrorException("Duplicate variable name: " + cur.theVar)
      }
      accum + (cur.theVar -> cur.typ)
    })
  }

  def makeEnvWithScopeLevel(args: Seq[FormalArg], scopeLevel: Int): Map[Variable, (Type, Int)] = {
    makeEnv(args).mapValues(typ => (typ -> scopeLevel))
  }
  
  def makeStructMap(prog: Program): Map[StructName, Map[Variable, Type]] = {
    prog.structs.foldLeft(Map[StructName, Map[Variable, Type]]())((accum, cur) => {
      val StructDef(name, args) = cur
      if (accum.contains(name)) {
        throw TypeErrorException("Struct redefined: name")
      }
      accum + (name -> makeEnv(args))
    })
  }

  def makeFunctionMap(prog: Program): Map[FunctionName, (Seq[Type], Type)] = {
    prog.funcs.foldLeft(Map[FunctionName, (Seq[Type], Type)]())((accum, cur) => {
      val Func(returnType, name, args, _) = cur
      if (accum.contains(name)) {
        throw TypeErrorException("Duplicate function name: " + name)
      }
      accum + (name -> (args.map(_.typ), returnType))
    })
  }

  def typecheckProgram(prog: Program): Unit = {
    val functionMapping = makeFunctionMap(prog)
    val structMapping = makeStructMap(prog)
    prog.funcs.foreach(func => typecheckFunc(func, functionMapping, structMapping))
  }

  // funcsWithOverloading: Map[(FunctionName, Seq[Type]), Type]
  def typecheckFunc(
    func: Func,
    funcs: Map[FunctionName, (Seq[Type], Type)],
    structs: Map[StructName, Map[Variable, Type]]): Unit = {
    val (_, returnAnalysis) =
      typecheck(
        func.body,
        0,
        makeEnvWithScopeLevel(func.args, 0),
        funcs,
        structs,
        func.returnType)
    returnAnalysis match {
      case DefinitelyReturns => ()
      case _ => {
        throw TypeErrorException("Function might not return: " + func)
      }
    }
  }

  def typecheckStmts(
    stmts: Seq[Stmt],
    scopeLevel: Int,
    env: Map[Variable, (Type, Int)],
    funcs: Map[FunctionName, (Seq[Type], Type)],
    structs: Map[StructName, Map[Variable, Type]],
    returnType: Type): (Map[Variable, (Type, Int)], ReturnAnalysis) = {
    stmts.foldLeft((env, NoReturn))((accum, curStmt) => {
      val (curEnv, curReturn) = accum
      val (nextEnv, stmtReturn) = typecheck(curStmt, scopeLevel, curEnv, funcs, structs, returnType)
      val nextReturn = (curReturn, stmtReturn) match {
        case (NoReturn, DefinitelyReturns) => DefinitelyReturns
        case (NoReturn, MaybeReturns) => MaybeReturns
        case (NoReturn, NoReturn) => NoReturn
        case (MaybeReturn, NoReturn) => MaybeReturn
        case (MaybeReturn, MaybeReturn) => MaybeReturn
        case (MaybeReturn, DefinitelyReturns) => DefinitelyReturns
        case (DefinitelyReturns, _) => throw TypeErrorException("Dead code: " + curStmt)
      }
      (nextEnv, nextReturn)
    })
  }

  def assertTypesSame(expected: Type, received: Type): Unit = {
    if (expected != received) {
      throw TypeErrorException(
        "Expected type: " + expected +
          "; received type: " + received)
    }
  }

  // TODO: finish first-order functions, go higher-order
  def typecheck(
    stmt: Stmt,
    scopeLevel: Int,
    env: Map[Variable, (Type, Int)],
    funcs: Map[FunctionName, (Seq[Type], Type)],
    structs: Map[StructName, Map[Variable, Type]],
    returnType: Type): (Map[Variable, (Type, Int)], ReturnAnalysis) = {
    stmt match {
      case ReturnStmt(exp) => {
        assertTypesSame(returnType, typeOf(exp, env, funcs, structs))
        (env, DefinitelyReturns)
      }
      case PrintlnStmt(exp) => {
        typeof(exp, env, funcs, structs)
        (env, NoReturn)
      }
      // expectedType theVar = initializer;
      case VariableDeclarationStmt(expectedType, theVar, initializer) => {
        val receivedType = typeof(initializer, env, funcs, structs)
        assertTypesSame(expectedType, receivedType)
        env.get(theVar) match {
          case Some((_, `scopeLevel`)) =>
            throw TypeErrorException("Name in same scope: " + theVar)
          case _ =>
            (env + (theVar -> (receivedType, scopeLevel)), NoReturn)
        }
      }
      case BlockStmt(stmts) => {
        val (_, returnAnalysis) =
          typecheckStmts(stmts, scopeLevel + 1, env, funcs, structs, returnType)
        (env, returnAnalysis)
      }
    }
  }

  def typeof(
    exp: Exp,
    env: Map[Variable, (Type, Int)],
    funcs: Map[FunctionName, (Seq[Type], Type)],
    structs: Map[StructName, Map[Variable, Type]]): Type = {
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
        val leftType: Type = typeof(left, env, funcs, structs)
        val rightType: Type = typeof(right, env, funcs, structs)
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
      } // BinopExp
      case CallExp(name, params) => {
        val actualParamTypes = params.map(param => typeof(param, env, funcs, structs))
        funcs.get(name) match {
          case Some((expectedParamTypes, returnType)) => {
            if (actualParamTypes != expectedParamTypes) {
              throw TypeErrorException("Call had incorrect params")
            }
            returnType
          }
          case None => throw TypeErrorException("No such function: " + name)
        }
      } // CallExp
      case MakeStructExp(name, fields) => {
        structs.get(name) match {
          case Some(expectedFieldTypes: Map[Variable, Type]) => {
            val types: Seq[(Variable, Type)] =
              fields.map(pair => (pair._1, typeof(pair._2, env, funcs, structs)))
            val asMap: Map[Variable, Type] = types.toMap
            if (asMap.size != types.size) {
              throw TypeErrorException("Duplicate names when creating struct")
            }
            if (asMap != expectedFieldTypes) {
              throw TypeErrorException("Something wrong with fields")
            }
            StructType(name)
          }
          case None => throw TypeErrorException("No such struct: " + name)
        }
      } // MakeStructExp
      case DotExp(exp, variable) => {
        typeof(exp, env, funcs, structs) match {
          case StructType(name) => {
            structs.get(name) match {
              case Some(fields) => {
                fields.get(variable) match {
                  case Some(fieldType) => fieldType
                  case None => throw TypeErrorException("Field doesn't exist: " + variable)
                }
              }
              case None => throw TypeErrorException("Struct doesn't exist: " + name)
            }
          }
        }
      } // DotExp
    }
    exp.typ = expType
    expType
  }
}

case class TypeErrorException(msg: String) extends Exception(msg)

