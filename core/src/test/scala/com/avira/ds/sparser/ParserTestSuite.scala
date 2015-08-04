package com.avira.ds.sparser

import org.scalatest.WordSpec

abstract class ParserTestSuite[I, O] extends WordSpec {

  def parser: Parser[I, O]

  case class ParserTest(
      name: String,
      input: I,
      expectedValue: ExpectedValueOption[O],
      expectedErrors: ExpectedErrors = ExpectedErrors()) {

    s"""Parsing input "$name"""" should {
      val ParseResult(actualValueOption, errors, _) = parser.parse(input)
      val errorNames = errors.map(_.name).toSet

      // Check field values.
      actualValueOption.foreach { actualValue =>
        for (FieldMatch(fieldName, select, expectedFieldValue) <- expectedValue.fieldValues) {
          s"""extract field $fieldName""" in {
            assert(
              select(actualValue) == expectedFieldValue
            )
          }
        }
      }

      // Check errors.
      for (expectedErrorName <- expectedErrors.errorNames) {
        s"""report error "$expectedErrorName"""" in {
          assert(errorNames.contains(expectedErrorName))
        }
      }
    }
  }
}

sealed abstract class ExpectedValueOption[O] {
  def fieldValues: Seq[FieldMatch[O]]
}
case object ExpectedNoValue extends ExpectedValueOption[Nothing] {
  override val fieldValues: Seq[FieldMatch[Nothing]] = Seq()
}
case class ExpectedValue[O](
    override val fieldValues: FieldMatch[O]*) extends ExpectedValueOption[O]

case class ExpectedErrors(errorNames: String*)


