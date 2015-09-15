package com.avira.ds.sparser.test

import com.avira.ds.sparser._
import org.scalatest.WordSpec

import scala.reflect.ClassTag

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
abstract class ParserTestSuite[I, O : ClassTag] extends WordSpec {
  import ParserTestSuite._

  /** Instance of the parser you want to test */
  val parser: Parser[I, O]

  /** Defines a test case for a particular input passed to the parser and the
    * expected outcome.
    *
    * For each input you want to test define a [[ParserTestSuite#ParserTest]]
    * instance inside your [[ParserTestSuite]] extended test class. Use a short
    * descriptive name which should fit as ''<name>'' in phrase ''Parsing input
    * "<name>" should extract field "<field>"''. To define the expected outcome
    * for the input use one of the case classes of [[ExpectedResult]].
    *
    * You may want to transform (map) parser's actual output value before you
    * match it. In order to to this you can use `mapOutput` partial function. A
    * typical use case when you need to do this is when you want to "cast" the
    * output value to a more specific class. Since [[ExpectedResult]] is
    * invariant with respect to parser's (`Parser[I, O]`) output value (`O`) you
    * can define a partial function such as
    * `{ case o: MoreSpecificClass => o }` to do the cast, which will expose
    * `MoreSpecificClass` members in `FieldMatch#selectField`function.
    *
    * @param name Short and descriptive name of the test case which will be
    * printed by ScalaTest when running the test suite
    * @param input Input for the parser which needs to be tested
    * @param expectedResult [[ExpectedResult]] object which defines the
    * expected outcome of the parser based on the input
    * @see [[ExpectedResult]], [[ExpectedValue]], [[ExpectedErrors]]
    */
  case class ParserTest[OO](
      name: String,
      input: I,
      mapOutput: PartialFunction[O, OO],
      expectedResult: ExpectedResult[OO]) {
    import ParserTest._

    s"""Parsing input "$name"""" should {
      val parseResult = parser.parse(input)
      val mappedParseResult = parseResult.transform { output =>
        if (mapOutput.isDefinedAt(output)) {
          TransformSuccess(mapOutput(output))
        } else {
          TransformFailure(UndefinedFunctionDomain)
        }
      }

      val ParseResult(actualValueOption, errors, _) = mappedParseResult
      val errorClasses = errors.map(_.getClass).toSet

//      if (mappedParseResult.errors.contains(UndefinedFunctionDomain)) {
//        fail("mapOutput partial function is not defined on the value extracted by parsing input " +
//            s""""$name", ${parseResult.valueOption.get}.""")
//      }

      "return a value for which mapOutput partial function is defined" in {
        if (mappedParseResult.errors.contains(UndefinedFunctionDomain)) {
          assert(false,
              "mapOutput partial function is not defined on the value extracted by parsing input " +
              s""""$name", ${parseResult.valueOption.get}.""")
        }
      }

      // Check if ExpectedResult case classes match their corresponding ParseResult case classes.
      "lead to the same result type" in {
        (expectedResult, mappedParseResult) match {
          case (ExpectedSuccessResult(_), ParseResult.Success(_, _)) => assert(true)
          case (ExpectedWarningResult(_, _), ParseResult.Warning(_, _, _)) => assert(true)
          case (ExpectedFailureResult(_), ParseResult.Failure(_, _)) => assert(true)
          case _ => assert(false,
            s"Expected ${getResultTypeName(expectedResult)} result, but " +
                s"${getResultTypeName(mappedParseResult)} was found with error(s) " +
                s"${mappedParseResult.errors.map(_.getClass.getName).mkString(", ")}.")
        }
      }

      // Check field values.
      actualValueOption.foreach { actualValue =>
        val fieldValues = expectedResult.expectedValueOption
            .fold(Seq[FieldMatch[OO]]())(_.fieldValues)
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

  object ParserTest {

    def apply(
        name: String,
        input: I,
        expectedResult: ExpectedResult[O]): ParserTest[O] =
      ParserTest(name, input, { case o: O => o }, expectedResult)

    private[ParserTest] case object UndefinedFunctionDomain extends ParseError {
      override val message: Option[String] = None
      override val args: Seq[Any] = Seq()
    }
  }
}

object ParserTestSuite {
  def getResultTypeName(expectedResult: ExpectedResult[_]): String = expectedResult match {
    case s: ExpectedSuccessResult[_] => "success"
    case w: ExpectedWarningResult[_] => "warning"
    case f: ExpectedFailureResult[_] => "failure"
  }

  def getResultTypeName(actualResult: ParseResult[_, _]): String = actualResult match {
    case s: ParseResult.Success[_, _] => "success"
    case w: ParseResult.Warning[_, _] => "warning"
    case f: ParseResult.Failure[_] => "failure"
  }
}

/** Defines the expected outcome of a [[ParserTestSuite#ParserTest]], which
  * tests a particular parser for a particular input.
  *
  * Normally, you shouldn't manipulate this abstract class directly, as it is
  * only used internally, when you define a `ParserTest` use one of the case
  * classes which inherits from `this`:
  *
  *  - [[ExpectedSuccessResult]]: to define the result of a successful parsing
  *  operation as a list of expected field values via ([[ExpectedValue]]
  *  object);
  *  - [[ExpectedWarningResult]]: to define the result of a partially
  *  successful parsing operation, which encountered some errors, as a list of
  *  expected field values (via [[ExpectedValue]] object) and a list of
  *  expected `ParseError` classes (via [[ExpectedErrors]] object);
  *  - [[ExpectedFailureResult]]: to define the result of a failed parsing
  *  operation as a list of expected `ParseError` classes (via
  *  [[ExpectedErrors]] object).
  *
  * @tparam O Output value type of the parser tested
  * @see [[ParserTestSuite]], [[ParserTestSuite#ParserTest]]
  */
sealed abstract class ExpectedResult[O](
    val expectedValueOption: Option[ExpectedValue[O]],
    val expectedErrors: ExpectedErrors)

/** Defines the result of a successful parsing operation as a list of expected
  * field values via [[ExpectedValue]] object.
  *
  * @tparam O Output value type of the parser tested
  * @see [[ParserTestSuite]], [[ParserTestSuite#ParserTest]]
  */
case class ExpectedSuccessResult[O](
    expectedValue: ExpectedValue[O])
  extends ExpectedResult[O](Some(expectedValue), ExpectedErrors())

/** Defines the result of a partially successful parsing operation, which
  * encountered some errors, as a list of expected field values via
  * [[ExpectedValue]] object and a list of expected `ParseError` classes via
  * [[ExpectedErrors]] object.
  *
  * @tparam O Output value type of the parser tested
  * @see [[ParserTestSuite]], [[ParserTestSuite#ParserTest]]
  */
case class ExpectedWarningResult[O](
    expectedValue: ExpectedValue[O],
    override val expectedErrors: ExpectedErrors)
  extends ExpectedResult[O](Some(expectedValue), expectedErrors)

/** Defines the result of a failed parsing operation as a list of expected
  * `ParseError` classes via [[ExpectedErrors]] object.
  *
  * @tparam O Output value type of the parser tested
  * @see [[ParserTestSuite]], [[ParserTestSuite#ParserTest]]
  */
case class ExpectedFailureResult[O](
    override val expectedErrors: ExpectedErrors)
  extends ExpectedResult[O](None, expectedErrors)


/** Defines a list of expected field values in an [[ExpectedResult]].
  *
  * @see [[ExpectedResult]] and `com.avira.ds.sparser.test.FieldMatch` from
  * ''sparser-macros'' module.
  */
case class ExpectedValue[O](fieldValues: FieldMatch[O]*)

/** Defines a list of expected errors returned while parsing an input,
  * as`com.avira.ds.sparser.ParseError` classes.
  *
  * @see [[ExpectedResult]]
  */
case class ExpectedErrors(errorClasses: Class[_ <: ParseError]*)

