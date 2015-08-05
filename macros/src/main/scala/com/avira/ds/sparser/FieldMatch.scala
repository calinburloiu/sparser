package com.avira.ds.sparser

import scala.language.experimental.macros
import scala.reflect.macros.Context

case class FieldMatch[O](
    fieldName: String,
    selectField: O => Any,
    expectedFieldValue: Any)

object FieldMatch {

  def apply[O](selectField: O => Any, expectedFieldValue: Any): FieldMatch[O] = macro applyImpl[O]

  def applyImpl[O](c: Context)(
      selectField: c.Expr[O => Any],
      expectedFieldValue: c.Expr[Any]): c.Expr[FieldMatch[O]] = {
    import c.universe._

    val funcBodyRepExpr = selectFieldToStringImpl(c)(selectField)

    reify {
      FieldMatch(funcBodyRepExpr.splice, selectField.splice, expectedFieldValue.splice)
    }
  }

  def selectFieldToString[O](selectField: O => Any): String = macro selectFieldToStringImpl[O]

  def selectFieldToStringImpl[O](c: Context)(selectField: c.Expr[O => Any]): c.Expr[String] = {
    import c.universe._

    val funcBodyRep = selectField.tree match {
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
