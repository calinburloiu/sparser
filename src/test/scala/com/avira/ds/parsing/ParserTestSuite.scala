package com.avira.ds.parsing

import org.scalatest.WordSpec
import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag

abstract class ParserTestSuite[I, O: ClassTag] extends WordSpec {

  def parser: Parser[I, O]

  def tests: Seq[ParserTest[I]]

  start()

  private val _mirror = ru.runtimeMirror(getClass.getClassLoader)

  def start(): Unit = {
    parser.getClass.getSimpleName when {
      for (test <- tests) {
        s"parsing ${test.name}" should {
          val ParseResult(valueOption, errors, _) = parser.parse(test.input)
          val errorNames = errors.map(_.name).toSet

          // Check field values.
          valueOption.foreach { value =>
            for (expectation <- test.expectedOutput.fields) {
              s"extract field ${expectation._1}" in {
                assert(getFieldValue(value, expectation._1) == expectation._2)
              }
            }
          }

          // Check errors.
          for (expectedErrorName <- test.expectedErrors.errorNames) {
            s"report error $expectedErrorName" in {
              assert(errorNames.contains(expectedErrorName))
            }
          }
        }
      }
    }
  }

  private def getFieldValue(obj: O, fieldName: String): Any = {
    val fieldTermSymbol = ru.typeOf[SamplePerson].declaration(ru.newTermName(fieldName)).asTerm
    val objMirror = _mirror.reflect(obj)
    val fieldMirror = objMirror.reflectField(fieldTermSymbol)
    fieldMirror.get
  }
}

object ParserTestSuite {

}

case class ParserTest[I](
    name: String,
    input: I,
    expectedOutput: PotentialExpectedValue,
    expectedErrors: ExpectedErrors = ExpectedErrors())

sealed abstract class PotentialExpectedValue {
  def fields: Seq[(String, Any)]
}
case object NoExpectedValue extends PotentialExpectedValue {
  override val fields: Seq[(String, Any)] = Seq()
}
case class ExpectedValue(override val fields: (String, Any)*) extends PotentialExpectedValue

case class ExpectedErrors(errorNames: String*)