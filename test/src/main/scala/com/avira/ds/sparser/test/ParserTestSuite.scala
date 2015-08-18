package com.avira.ds.sparser.test

import com.avira.ds.sparser.{ParseError, ParseResult, Parser}
import org.scalatest.WordSpec

/** Extend this class for testing a `com.avira.ds.sparser.Parser` based on
  * expected outcomes for different input value.
  *
  * In order to define a test suite for your parser follow this steps:
  *
  *  - Extend this class.
  *  - Implement the abstract member `parser` which instantiates an instance of
  *  your parser.
  *  - Inside your extended class create a [[ParserTestSuite#ParserTest]] instance
  * for each input case you want to test.
  *
  * [[ParserTestSuite#ParserTest]] will
  * allow you to name that input case and define expected outcomes. Your build
  * tool (e.g. Maven, SBT etc.) will report errors for each expected output.
  * Here is an example usage:
  *
  * {{{
  * class SamplePersonParserTestSuite
  *     extends ParserTestSuite[String, SamplePerson] {
  * 
  *   override val parser: Parser[String, SamplePerson] = new SamplePersonParser(ParserConf())
  * 
  *   ParserTest("Good input",
  *     "John Doe\t33",
  *     ExpectedSuccessResult(
  *       ExpectedValue(
  *         FieldMatch(_.name, "John Doe"),
  *         FieldMatch(_.age, 33)
  *       )
  *     )
  *   )
  * }
  * }}}
  *
  * For more details on how to define tests check [[ParserTestSuite#ParserTest]].
  *
  * This class relies on `org.scalatest.WordSpec` from ScalaTest library which
  * facilitates a "behavior-driven" style of development (BDD).
  *
  * There are some usage samples in the source code of this library. Check them!
  *
  * @tparam I Input type of the parser tested
  * @tparam O Output value type of the parser tested
  * @see [[ParserTestSuite#ParserTest]]
  */
abstract class ParserTestSuite[I, O] extends WordSpec {

  /** Instance of the parser you want to test */
  val parser: Parser[I, O]

  /** Defines a test case for a particular input passed to the parser and the
    * expected outcome.
    *
    * For each input you want to test define a [[ParserTestSuite#ParserTest]] instance inside your [[ParserTestSuite]] extended test class. Use a short descriptive name which should fit as ''<name>'' in phrase ''Parsing input "<name>" should extract field "<field>"''. To define the expected outcome for the input use one of the case classes of [[ExpectedResult]] as follows:
    *
    *  - [[ExpectedSuccessResult]]: to define a list of expected field values via [[ExpectedValue]] object;
    *  - [[ExpectedWarningResult]]: to define a list of expected field values (via [[ExpectedValue]] object) and a list of expected `ParseError` classes (via [[ExpectedErrors]] object);
    *  - [[ExpectedFailureResult]]: to define a list of expected `ParseError` classes (via [[ExpectedErrors]] object).
    *
    * @param name Short and descriptive name of the test case which will be
    * printed by ScalaTest when running the test suite
    * @param input Input for the parser which needs to be tested
    * @param expectedResult [[ExpectedResult]] object which defines the
    * expected outcome of the parser based on the input
    * @see [[ExpectedResult]], [[ExpectedValue]], [[ExpectedErrors]]
    */
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

