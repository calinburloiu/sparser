package com.avira.ds.sparser

import com.avira.ds.sparser.sample.SamplePersonParser
import org.scalatest.WordSpec

abstract class ParserTestSuite[I, O] extends WordSpec {

  def parser: Parser[I, O]

  case class ParserTest(
      name: String,
      input: I,
      expectedResult: ExpectedResult[O]) {

    s"""Parsing input "$name"""" should {
      val ParseResult(actualValueOption, errors, _) = parser.parse(input)
      val errorClasses = errors.map(_.getClass).toSet

      // Check field values.
      actualValueOption.foreach { actualValue =>
        val fieldValues = expectedResult.expectedValueOption
          .fold(Seq[FieldMatch[O]]())(_.fieldValues)
        for (FieldMatch(fieldName, selectField, expectedFieldValue) <- fieldValues) {
          s"""extract field $fieldName""" in {
            assert(
              selectField(actualValue) == expectedFieldValue
            )
          }
        }

        if (fieldValues.isEmpty) {
          "output a value" in {
            assert(true)
          }
        }
      }

      // Check errors.
      for (expectedErrorClass <- expectedResult.expectedErrors.errorClasses) {
        s"""report error "${expectedErrorClass.getCanonicalName}"""" in {
          assert(errorClasses.contains(expectedErrorClass))
        }
      }
    }

    val s = Seq(classOf[SamplePersonParser.InvalidAgeParseError], classOf[ParseError])
    val x = ExpectedErrors(classOf[SamplePersonParser.InvalidAgeParseError], classOf[ParseError])
    val y = x.errorClasses
  }
}


sealed abstract class ExpectedResult[O](
  val expectedValueOption: Option[ExpectedValue[O]],
  val expectedErrors: ExpectedErrors)

case class ExpectedSuccessResult[O](
    expectedValue: ExpectedValue[O])
    extends ExpectedResult[O](Some(expectedValue), ExpectedErrors())

case class ExpectedWarningResult[O](
    expectedValue: ExpectedValue[O],
    override val expectedErrors: ExpectedErrors)
    extends ExpectedResult[O](Some(expectedValue), expectedErrors)

case class ExpectedFailureResult[O](
    override val expectedErrors: ExpectedErrors)
    extends ExpectedResult[O](None, expectedErrors)


case class ExpectedValue[O](fieldValues: FieldMatch[O]*)

case class ExpectedErrors(errorClasses: Class[_ <: ParseError]*)


