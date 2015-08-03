package com.avira.ds.parsing

import org.scalatest.WordSpec
import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag

abstract class ParserTestSuiteOld[I, O: ru.TypeTag : ClassTag] extends WordSpec {

  def parser: Parser[I, O]

  def tests: Seq[ParserTest[I]]

  start()

  private val _mirror = ru.runtimeMirror(getClass.getClassLoader)

  def start(): Unit = {
    for (test <- tests) {
      s"""Parsing input "${test.name}"""" should {
        val ParseResult(actualValueOption, errors, _) = parser.parse(test.input)
        val errorNames = errors.map(_.name).toSet

        // Check field values.
        actualValueOption.foreach { actualValue =>
          for ((fieldName, expectedFieldValue) <- test.expectedValue.fields) {
            s"""extract field "$fieldName"""" in {
              assert(getFieldValue(actualValue, fieldName) == expectedFieldValue)
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

  private def getFieldValue[T : ru.TypeTag : ClassTag](obj: T, fieldName: String): Any = {
    val fieldTermSymbol = getTypeTag(obj).tpe.declaration(ru.newTermName(fieldName)).asTerm
    val objMirror = _mirror.reflect(obj)
    val fieldMirror = objMirror.reflectField(fieldTermSymbol)
    fieldMirror.get
  }

  def getTypeTag[T : ru.TypeTag](obj: T): ru.TypeTag[T] = ru.typeTag[T]
}

object ParserTestSuiteOld {

}

case class ParserTest[I](
    name: String,
    input: I,
    expectedValue: PotentialExpectedValue,
    expectedErrors: ExpectedErrors = ExpectedErrors())

sealed abstract class PotentialExpectedValue {
  def fields: Seq[(String, Any)]
}
case object NoExpectedValue extends PotentialExpectedValue {
  override val fields: Seq[(String, Any)] = Seq()
}
case class ExpectedValue(override val fields: (String, Any)*) extends PotentialExpectedValue

case class ExpectedErrors(errorNames: String*)
