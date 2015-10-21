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
package com.avira.ds.sparser.samples

import com.avira.ds.MacroUtils
import com.avira.ds.sparser._

import scala.util.{Failure, Success, Try}

/** Sample case class description a person used as the output value of
  * [[SamplePersonParser]].
  *
  * @param name Name of the person
  * @param age Age of the person
  */
case class SamplePerson(
    name: String,
    age: Int)

/** Sample parser implemented to show how trait [[com.avira.ds.sparser.Parser]] should be
  * implemented for simple TSV input.
  *
  * Check [[com.avira.ds.sparser.Parser]] for a usage example.
  *
  * @param conf a [[com.avira.ds.sparser.ParserConf]] instance.
  */
class SamplePersonParser(override val conf: ParserConf)
    extends Parser[String, SamplePerson] {
  import SamplePersonParser._

  override def parse(
      inputResult: ParseResult[String, String]): ParseResult[String, SamplePerson] = {
    val splitResult: ParseResult[String, (String, String)] =
      // Split the TSV row into columns.
      inputResult.transform { line: String =>
        val cols = line.split("\t")
        cols.length match {
          case x: Int if x < 2 =>
            TransformFailure(
              NotEnoughColumnsParseError(cols.length)
            )
          case x: Int if x > 2 =>
            val (rawName, rawAge) = (cols(0), cols(1))
            TransformWarning(
              (rawName, rawAge),
              TooManyColumnsParseError(cols.length)
            )
          case x: Int =>
            val Array(rawName, rawAge) = cols
            TransformSuccess(
              (rawName, rawAge)
            )
        }
      }

    // Parse the age value.
    splitResult.transform {
      case (rawName, rawAge) => Try(rawAge.toInt) match {
        case Success(age) if age >= 0 =>
          TransformSuccess(
            SamplePerson(rawName, age)
          )
        case Failure(e: NumberFormatException) =>
          TransformWarning(
            SamplePerson(rawName, -1),
            InvalidAgeParseError(rawAge, e)
          )
      }
    }
  }
}

/** Companion of [[SamplePersonParser]] which defines
  * [[com.avira.ds.sparser.ParseError]]s that be reported by the parser.
  */
object SamplePersonParser {

  sealed abstract class SamplePersonParseError(
      override val message: Option[String],
      override val args: Seq[Any]) extends ParseError

  case class InvalidAgeParseError(rawAge: String, e: NumberFormatException)
      extends SamplePersonParseError(Some(s"Invalid age $rawAge"), Seq(e))

  case class TooManyColumnsParseError(colsCount: Int)
      extends SamplePersonParseError(Some("Too many columns"), Seq(colsCount))

  case class NotEnoughColumnsParseError(colsCount: Int)
      extends SamplePersonParseError(Some("Insufficient columns"), Seq(colsCount))

  /** Returns all [[ParseError]]s which could be reported by [[SamplePersonParser]]. */
  def parseErrorClasses: Set[Class[_ <: ParseError]] =
    MacroUtils.getSealedClassChildren[SamplePersonParseError]
      .asInstanceOf[Set[Class[_ <: ParseError]]]
}
