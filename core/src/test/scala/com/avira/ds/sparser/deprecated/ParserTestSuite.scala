package com.avira.ds.sparser.deprecated

import com.avira.ds.sparser.{ParseError, ParseResult, Parser}
import org.scalatest.WordSpec

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

@deprecated
abstract class ParserTestSuite[I, O: ru.TypeTag : ClassTag] extends WordSpec {

  def parser: Parser[I, O]

  def tests: Seq[ParserTest[I]]

  start()

  private val _mirror = ru.runtimeMirror(getClass.getClassLoader)

  def start(): Unit = {
    for (test <- tests) {
      s"""Parsing input "${test.name}"""" should {
        val ParseResult(actualValueOption, errors, _) = parser.parse(test.input)
        val errorClasses = errors.map(_.getClass).toSet

        // Check field values.
        actualValueOption.foreach { actualValue =>
          for ((fieldName, expectedFieldValue) <- test.expectedValue.fields) {
            s"""extract field "$fieldName"""" in {
              assert(getFieldValue(actualValue, fieldName) == expectedFieldValue)
            }
          }
        }

        // Check errors.
        for (expectedErrorClass <- test.expectedErrors.errorClasses) {
          s"""report error "${expectedErrorClass.getCanonicalName}"""" in {
            assert(errorClasses.contains(expectedErrorClass))
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

@deprecated
case class ParserTest[I](
    name: String,
    input: I,
    expectedValue: ExpectedValueOption,
    expectedErrors: ExpectedErrors = ExpectedErrors())


@deprecated
sealed abstract class ExpectedValueOption {
  def fields: Seq[(String, Any)]
}

@deprecated
case object ExpectedNoValue extends ExpectedValueOption {
  override val fields: Seq[(String, Any)] = Seq()
}
@deprecated
case class ExpectedValue(override val fields: (String, Any)*) extends ExpectedValueOption


@deprecated
case class ExpectedErrors(errorClasses: Class[_ <: ParseError]*)
