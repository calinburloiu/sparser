/*
 * Copyright 2015 Avira Operations GmbH
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package com.avira.ds.sparser.test.deprecated

import com.avira.ds.sparser.{ParseError, ParseResult, Parser}
import org.scalatest.WordSpec

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

@deprecated("Use the new com.avira.ds.sparser.test.ParserTestSuite", "0.1.0-SNAPSHOT")
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

@deprecated("Use the new com.avira.ds.sparser.test.ParserTestSuite", "0.1.0-SNAPSHOT")
case class ParserTest[I](
    name: String,
    input: I,
    expectedValue: ExpectedValueOption,
    expectedErrors: ExpectedErrors = ExpectedErrors())


@deprecated("Use the new com.avira.ds.sparser.test.ParserTestSuite", "0.1.0-SNAPSHOT")
sealed abstract class ExpectedValueOption {
  def fields: Seq[(String, Any)]
}

@deprecated("Use the new com.avira.ds.sparser.test.ParserTestSuite", "0.1.0-SNAPSHOT")
case object ExpectedNoValue extends ExpectedValueOption {
  override val fields: Seq[(String, Any)] = Seq()
}
@deprecated("Use the new com.avira.ds.sparser.test.ParserTestSuite", "0.1.0-SNAPSHOT")
case class ExpectedValue(override val fields: (String, Any)*) extends ExpectedValueOption


@deprecated("Use the new com.avira.ds.sparser.test.ParserTestSuite", "0.1.0-SNAPSHOT")
case class ExpectedErrors(errorClasses: Class[_ <: ParseError]*)
