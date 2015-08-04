package com.avira.ds.parsing.neo

import com.avira.ds.parsing.sample.{SamplePerson, SamplePersonParser}
import com.avira.ds.parsing.{ParseResult, ParserConf, Parser}
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

case class FieldMatch[O](fieldName: String, select: O => Any, expectedFieldValue: Any)
object FieldMatch {
  def apply[O](select: O => Any, expectedFieldValue: Any): FieldMatch[O] =
    FieldMatch(s"unknown${select.hashCode()}", select, expectedFieldValue)
}

case class ExpectedErrors(errorNames: String*)

class SamplePersonParserTestSuite extends ParserTestSuite[String, SamplePerson] {
  override lazy val parser: Parser[String, SamplePerson] = new SamplePersonParser(ParserConf())

  ParserTest("Good",
    "Calin\t28",
    ExpectedValue(
      FieldMatch("name", _.name, "Calin"),
      FieldMatch(_.age, 28)
    )
  )
}
