package com.avira.ds.sparser.test

import scala.language.experimental.macros
import scala.reflect.macros.Context

/** A matching rule in a parser test.
  *
  * A particular field value selected by `selectField` function must match the
  * `expectedFieldValue`.
  *
  * The companion object contains a convenience `apply` method which will
  * create an instance with `fieldName` deducted from `selectField`.
  *
  * @param fieldName name of the field to match
  * @param selectField function which selects the field from a result value object
  * @param expectedFieldValue expected value for the field
  * @tparam O type of the parser result value
  */
case class FieldMatch[O](
    fieldName: String,
    selectField: O => Any,
    expectedFieldValue: Any)

/** Factories for creating `FieldMatch` instances and some helper macros. */
object FieldMatch {

  /** Creates a [[FieldMatch]] by guessing `fieldName` from `selectField`.
    *
    * @param selectField function which select the field from a result value object
    * @param expectedFieldValue expected value for the field
    * @tparam O type of the parser result value
    * @return a [[FieldMatch result]]
    */
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

  /** Generates `fieldName` String from a `selectField` function. */
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

