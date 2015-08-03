package com.avira.ds.parsing.neo

import com.avira.ds.parsing.sample.{SamplePerson, SamplePersonParser}
import com.avira.ds.parsing.{ParseResult, ParserConf, Parser}
import org.scalatest.WordSpec

abstract class ParserTestSuite[I, O] extends WordSpec {

  def parser: Parser[I, O]

  def tests: Seq[ParserTest[I, O]]

  start()

  def start(): Unit = {
    for (test <- tests) {
      s"""Parsing input "${test.name}"""" should {
        val ParseResult(actualValueOption, errors, _) = parser.parse(test.input)
        val errorNames = errors.map(_.name).toSet

        // Check field values.
        actualValueOption.foreach { actualValue =>
          for ((actualFieldValueFunc, expectedFieldValue) <- test.expectedValue.fieldValues) {
            s"""extract field unknown${actualFieldValueFunc.hashCode()}""" in {
              assert(
                actualFieldValueFunc(actualValue) == expectedFieldValue
              )
            }
          }
        }

        // Check errors.
        for (expectedErrorName <- test.expectedErrors.errorNames) {
          s"""report error "$expectedErrorName"""" in {
            assert(errorNames.contains(expectedErrorName))
          }
        }
      }
    }
  }
}

case class ParserTest[I, O](
    name: String,
    input: I,
    expectedValue: PotentialExpectedValue[O],
    expectedErrors: ExpectedErrors = ExpectedErrors())

sealed abstract class PotentialExpectedValue[O] {
  def fieldValues: Seq[(O => Any, Any)]
}
case object NoExpectedValue extends PotentialExpectedValue[Nothing] {
  override val fieldValues: Seq[(Nothing => Any, Any)] = Seq()
}
case class ExpectedValue[O](
    override val fieldValues: (O => Any, Any)*) extends PotentialExpectedValue[O]

case class ExpectedErrors(errorNames: String*)

class SamplePersonParserTestSuite extends ParserTestSuite[String, SamplePerson] {
  override lazy val parser: Parser[String, SamplePerson] = new SamplePersonParser(ParserConf())

  override lazy val tests: Seq[ParserTest[String, SamplePerson]] = Seq(
    ParserTest("Good",
      "Calin\t28",
      ExpectedValue(
        (_.name, "Calin"),
        (_.age, 28)
      )
    )
  )
}
