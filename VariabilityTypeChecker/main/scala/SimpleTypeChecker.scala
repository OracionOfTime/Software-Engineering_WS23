/** Type checking without variability.
  */
package de.uni_saarland.cs.se

import Constant.*
import Expression.*
import Type.*

/** Type context as in lecture slides chapter 6 slide 71.
  *
  * @tparam IdT
  *   type used for variables
  * @tparam TypeT
  *   the types of the language
  */
case class TypeContext[IdT, TypeT] private (mapping: Map[IdT, TypeT]) {

  /** Create an extended copy of this type context that sets the type for the
    * given variable.
    */
  def withVar(id: IdT, value: TypeT): TypeContext[IdT, TypeT] = {
    new TypeContext(mapping updated (id, value))
  }

  /** Get the type for a given variable.
    */
  def typeForVar(id: IdT): Option[TypeT] = mapping.get(id)

  override def toString: String = {
    mapping.toSeq
      .map({ case (id: IdT, t: TypeT) =>
        s"($id: $t)"
      })
      .mkString("\n")
  }
}

object TypeContext {

  /** Magic function so that we can write `TypeContext(("x", BoolTy))` instead
    * of `new TypeContext(Map("x", BoolTy))`.
    */
  def apply[IdT, TypeT](elems: (IdT, TypeT)*): TypeContext[IdT, TypeT] =
    new TypeContext(Map.from(elems))
}

/** Type alias for context type, i.e., the type context. */
type Context = TypeContext[Identifier, Type]

/** Type alias for result type. */
type Result = TypeCheckResult[Expression, Type, Context]

object SimpleTypeChecker {

  /** Type-check a single expression.
    */
  def checkType(
      expr: Expression,
      context: Context = TypeContext()
  ): Result = {
    new SimpleTypeChecker().checkType(expr, context)
  }
}

/** Type checker implementation for the language without variability.
  */
class SimpleTypeChecker extends TypeChecker[Expression, Type, Context] {

  override def checkType(expr: Expression, context: Context): Result = {
    expr match {

      case Const(c) =>
        val ty = c match {
          case True | False => BoolTy
          case Num(_) => NumTy
        }
        Success(ty)

      case Id(id) =>
        context.typeForVar(id) match {
          case Some(ty) => Success(ty)
          case None => Failure(expr, context, s"$id not in context")
        }

      case Smaller(lhs, rhs) =>
        (checkType(lhs, context), checkType(rhs, context)) match {
          case (Success(NumTy), Success(NumTy)) => Success(BoolTy)
          case (Failure(_, _, mess), _) => Failure(expr, context, mess)
          case (_, Failure(_,_, mess)) => Failure(expr,context, mess)

          case (Success(lhsT), _) => Failure(expr, context, s"$lhs is not of type NumTy, but $lhsT")
          case (_, Success(rhsT)) => Failure(expr, context, s"$rhs is not of type NumTy, but $rhsT")
       }

      case If(condition, thenExpr, elseExpr) =>
        (checkType(condition, context), checkType(thenExpr, context), checkType(elseExpr, context)) match {
          case (Failure(_,_, mess),_,_) => Failure(expr, context, mess)
          case (_, Failure(_,_, mess),_) => Failure(expr, context, mess)
          case (_,_, Failure(_,_, mess)) => Failure(expr, context, mess)

          case (Success(BoolTy), Success(thenExprT), Success(elseExprT)) =>
            if(thenExprT == elseExprT) {
              Success(elseExprT)
            } else {
              Failure(expr, context, s"$thenExpr and $elseExpr are not of same type as expected, but $thenExprT, $elseExprT")
            }

          case (Success(condT), Success(_), Success(_)) =>
            Failure(expr, context, s"$condition is not of type Boolean, but $condT")
        }

      case Let(variable, varValue, inExpr) =>
        context.typeForVar(variable) match {
          case Some(_) => Failure(expr, context, s"chosen variable $variable is already defined within the context")
          case None => checkType(varValue, context) match {
            case Success(varValT) => checkType(inExpr, context.withVar(variable, varValT))
            case Failure(_,_, mess) => Failure(expr, context, mess)
          }
        }
      }
  }
}
