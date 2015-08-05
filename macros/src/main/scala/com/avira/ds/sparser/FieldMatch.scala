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

    // Extract field definition from function selectField.
    val selectFieldBodyRepExpr = selectFieldToStringImpl(c)(selectField)

    // Generate AST of FieldMatch default apply with the extract fieldName (field definition).
    reify {
      FieldMatch(selectFieldBodyRepExpr.splice, selectField.splice, expectedFieldValue.splice)
    }
  }

  def selectFieldToString[O](selectField: O => Any): String = macro selectFieldToStringImpl[O]

  def selectFieldToStringImpl[O](c: Context)(selectField: c.Expr[O => Any]): c.Expr[String] = {
    import c.universe._

    // Extract field definition from function body.
    val selectFieldBodyRep = selectField.tree match {
      case Function(_, body) =>
        val showOutput = show(body)

        // Transform function body into a pretty field definition.
        showOutput
          // Remove prefix; e.g. x$1.field ==> field:
          .substring(showOutput.indexOf('.') + 1)
          // Remove ".apply"; e.g. friends.apply(2) ==> friends(2):
          .replace(".apply", "")
    }

    // Create AST for the field definition string.
    val selectFieldBodyRepTree = Literal(Constant(selectFieldBodyRep))
    val selectFieldBodyRepExpr = c.Expr[String](selectFieldBodyRepTree)
    reify { selectFieldBodyRepExpr.splice }
  }
}
