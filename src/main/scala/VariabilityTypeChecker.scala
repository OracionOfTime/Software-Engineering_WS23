/** Type checking with variability.
  */
package de.uni_saarland.cs.se

import Constant.*
import Type.*
import VExpression.*

import cafesat.api.Formulas.Formula
import cafesat.api.{Formulas, Solver}

/** Variability context as in lecture slides chapter 6 slide 74.
  */
case class VariabilityContext(formula: Formula) {

  /** Make equality consider logical equality of formulas.
    */
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: VariabilityContext =>
        Solver.solveForSatisfiability(!(formula iff other.formula)) match {
          case Some(_) => false
          case None    => true
        }
      case _ => false
    }
  }

  override def toString: String = formula.toString
}

object VariabilityContext {

  /** Creates an empty variability context.
    */
  def emptyContext(): VariabilityContext = VariabilityContext(Formulas.True)

  /** Allow implicit conversion from formulas to `VariabilityContext`.
    */
  given formulaToVarCtx: Conversion[Formula, VariabilityContext] =
    VariabilityContext(_)
}

/** Type alias for type context type */
type VTypeContext = TypeContext[Identifier, VType]

/** Type alias for context (= variability context + type context) type */
type VContext = (VariabilityContext, VTypeContext)

/** Type alias for result type */
type VResult = TypeCheckResult[VExpression, VType, VContext]

object VariabilityTypeChecker {

  /** Type-check a single expression.
    */
  def checkType(
      expr: VExpression,
      context: VContext = createContext()
  ): VResult = {
    new VariabilityTypeChecker().checkType(expr, context)
  }

  /** Simplify creation of variability context + type context.
    */
  def createContext(
      variabilityContext: VariabilityContext =
        VariabilityContext.emptyContext(),
      typeContext: VTypeContext = TypeContext()
  ): VContext = (variabilityContext, typeContext)
}

/** Type checker implementation for the language with variability.
  */
class VariabilityTypeChecker extends TypeChecker[VExpression, VType, VContext] {

  override def checkType(expr: VExpression, context: VContext): VResult = {
    val variabilityContext = context._1
    val vTypeContext = context._2
    expr match {
      case Const(c) =>
        val ty = c match {
          case True | False => BoolTy
          case Num(_) => NumTy
        }
        Success(VType(ty -> variabilityContext.formula))


      case Id(id) =>
        vTypeContext.typeForVar(id) match {
          case None => Failure(expr, context, s"identifier $id does not exist in the given type context")
          case Some(varType) =>
            val t = theta(varType)
            Solver.solveForSatisfiability(
              !variabilityContext.formula || t
            ) match {
              case None => Failure(expr, context, s"type of $id is not well-defined in set of VTypes")
              case Some(_) =>
                Success(
                  new VType(updateTau(varType, variabilityContext, expr))
                )
            }
        }

      case Smaller(lhs, rhs) =>
        (checkType(lhs, context), checkType(rhs, context)) match {
          case (Success(lhsVty), Success(rhsVty)) =>
            if(!(lhsVty.equals(new VType(Map(NumTy -> variabilityContext.formula))) &&
            rhsVty.equals(new VType(Map(NumTy -> variabilityContext.formula))))) {
              return Failure(expr, context, s"$lhsVty and/or $rhsVty are not of correct types")
            }
//            val leftSize = lhsVty.types.size
//            val rightSize = rhsVty.types.size
//            if(leftSize > 1 || rightSize > 1) {
//              return Failure(expr, context, s"Either $lhsVty has too many types or $rhsVty does, thus precondition might be true or false, specify please to choose correct features")
//            }
            (lhsVty.formulaForType(NumTy), rhsVty.formulaForType(NumTy)) match {
              case (Some(lhsPhi), Some(rhsPhi)) =>
                Solver.solveForSatisfiability(
                  !variabilityContext.formula || (lhsPhi && rhsPhi)
                ) match {
                  case None =>
                    Failure(expr, context, s"types of $lhs and/or $rhs are/is not well-defined in set of VTypes")
                  case Some(_) =>
                    Success(VType(BoolTy -> variabilityContext.formula))
                }
              case (None,_) => Failure(expr, context, s"$lhs, $rhs or both not have a valid configuration for NumTy")
              case(_, None) => Failure(expr, context, s"$lhs, $rhs or both not have a valid configuration for NumTy")
            }
          case (Failure(_,_, mess),_) => Failure(expr, context, mess)
          case (_, Failure(_,_, mess)) => Failure(expr, context, mess)
          
          }


      case If(condition, thenExpr, elseExpr) =>
        (checkType(condition, context), checkType(thenExpr, context), checkType(elseExpr, context)) match {
          case (Failure(_, _, mess), _, _) => Failure(expr, context, mess)
          case (_, Failure(_, _, mess), _) => Failure(expr, context, mess)
          case (_, _, Failure(_, _, mess)) => Failure(expr, context, mess)

          case (Success(conditionVty), Success(thenVty), Success(elseVty)) =>
//            val sizeCondition = conditionVty.types.size
//            if(sizeCondition > 1) {
//              return Failure(expr, context, s"$conditionVty has too many types, specify the precondition")
//            }
            if(!(conditionVty.equals(new VType(Map(BoolTy -> variabilityContext.formula))))) {
              return Failure(expr, context, s"$conditionVty is not of correct type")
            }
            conditionVty.formulaForType(BoolTy) match {
              case Some(conditionPhi) =>
                Solver.solveForSatisfiability(
                  !variabilityContext.formula || conditionPhi
                ) match {
                  case None => Failure(expr, context, s"type of $condition is not well-defined in set of VTypes")
                  case Some(_) =>
                    val t1 = theta(thenVty)
                    val t2 = theta(elseVty)
                    Solver.solveForSatisfiability(
                      (!variabilityContext.formula || t1) && (!variabilityContext.formula || t2)
                    ) match {
                      case Some(_) =>
                        if(thenVty.equals(elseVty)) {
                          Success(new VType(merge(thenVty, elseVty)))
                        } else {
                          Failure(expr, context, s"$thenExpr and $elseExpr do not have the same VTypes")
                        }
                      case None => Failure(expr, context, s"type/s of either $thenExpr or/and $elseExpr is/are not well-defined in set of VTypes")
                    }
                }
              case None => Failure(expr, context, s"type of $condition is not Boolty, but $conditionVty")
            }
        }



      case Let(variable, varValue, inExpr) =>
        vTypeContext.typeForVar(variable) match {
          case Some(_) => Failure(expr, context, s"$variable already defined in domain of type context")
          case None =>
            checkType(varValue, context) match {
              case Failure(_,_, mess) => Failure(expr, context, mess)
              case Success(varValVty) =>
                val t1 = theta(varValVty)
                Solver.solveForSatisfiability(
                  !variabilityContext.formula || t1
                ) match {
                  case None => Failure(expr, context, s"type of $varValue is not well-defined in set of VTypes")
                  case Some(_) =>
                    checkType(inExpr, (variabilityContext, vTypeContext.withVar(variable, varValVty))) match {
                      case Failure(_,_,mess) => Failure(expr, context, mess)
                      case Success(inExprVty) =>
                        val t2 = theta(inExprVty)
                        Solver.solveForSatisfiability(
                          !(variabilityContext.formula && !t2)
                        ) match {
                          case None => Failure(expr, context, s"type of $inExpr is not well-defined in set of VTypes")
                          case Some(_) =>
                            Success(inExprVty)
                        }
                    }
                }
            }
        }


      case Choice(presenceCondition, trueChoice, falseChoice) =>
        (checkType(trueChoice, (variabilityContext.formula && presenceCondition, vTypeContext)),
          checkType(falseChoice, (variabilityContext.formula && !presenceCondition, vTypeContext))
        ) match {
          case (Success(trueChoiceVty), Success(falseChoiceVTy)) => Success(new VType(merge(trueChoiceVty, falseChoiceVTy)))
          case (_,Failure(_,_, mess)) => Failure(expr, context, mess)
          case (Failure(_,_, mess),_) => Failure(expr, context, mess)
        }
    }
  }



  private def theta(varType: VType): Formula = {
    val setOfTypes = varType.dom()
    var setOfFormulas: Set[Formulas.Formula] = Set()
    for (ty <- setOfTypes) {
      setOfFormulas += varType.formulaForType(ty).get
    }
    var phi = setOfFormulas.head
    setOfFormulas = setOfFormulas.drop(1)
    for(bf <- setOfFormulas) {
      phi = phi || bf
    }
    phi
  }

  private def updateTau(vType: VType, variabilityContext: VariabilityContext, expr: VExpression): Map[Type, Formula] = {
    var newTau: Map[Type, Formula] = Map()
    for(tau <- vType.types) {
          newTau = newTau + (tau._1 -> (variabilityContext.formula && tau._2))
      }
    newTau
  }

  private def merge(vType1: VType, vType2: VType): Map[Type, Formula] = {
    var crossJoin: Map[Type, Formula] = Map()
    val setOfTypes: Set[Type] = vType1.dom() ++ vType2.dom()

    for(ty <- setOfTypes) {
      (vType1.formulaForType(ty), vType2.formulaForType(ty)) match {
        case (Some(tau1), None) => crossJoin = crossJoin + (ty -> tau1)
        case (None, Some(tau2)) => crossJoin = crossJoin + (ty -> tau2)
        case (Some(tau1), Some(tau2)) => crossJoin = crossJoin + (ty -> (tau1 || tau2))
      }
    }
    crossJoin
  }
}
