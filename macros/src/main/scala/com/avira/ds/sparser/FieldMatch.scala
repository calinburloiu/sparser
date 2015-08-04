package com.avira.ds.sparser

import scala.language.experimental.macros
import scala.reflect.macros.Context

case class FieldMatch[O](
    fieldName: String,
    select: O => Any,
    expectedFieldValue: Any)

object FieldMatch {

  def apply[O](select: O => Any, expectedFieldValue: Any): FieldMatch[O] = macro applyImpl[O]

  def applyImpl[O: c.WeakTypeTag](c: Context)(
      select: c.Expr[O => Any],
      expectedFieldValue: c.Expr[Any]): c.Expr[FieldMatch[O]] = {
    import c.universe._

    val funcBodyRepExpr = selectToStringImpl(c)(select)

    reify {
      FieldMatch(funcBodyRepExpr.splice, select.splice, expectedFieldValue.splice)
    }
  }

  def selectToString[O](f: O => Any): String = macro selectToStringImpl[O]

  def selectToStringImpl[O](c: Context)(f: c.Expr[O => Any]): c.Expr[String] = {
    import c.universe._

    val funcBodyRep = f.tree match {
      case Function(_, s) =>
        val showOutput = show(s)
        // Remove prefix:
        showOutput.substring(showOutput.indexOf('.') + 1)
        // Remove ".apply":
          .replace(".apply", "")
    }
    val funcBodyRepTree = Literal(Constant(funcBodyRep))
    val funcBodyRepExpr = c.Expr[String](funcBodyRepTree)

    reify { funcBodyRepExpr.splice }
  }
}
