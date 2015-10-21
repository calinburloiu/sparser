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

/** Nested object sample returned as output by [[SampleNestedObjectParser]] */
case class NestedObject(
    a: String,
    b: String,
    c: Ratio)

/**
  * @see [[NestedObject]]
  */
case class Ratio(
    x: Int,
    y: Int)

/** Sample parser implemented to show how to test a
  * [[com.avira.ds.sparser.Parser]] implementation which outputs a nested
  * object.
  *
  * Check the tests to see how this kind of parsers can be tested.
  *
  */
class SampleNestedObjectParser extends Parser[String, NestedObject] {
  import SampleNestedObjectParser._

  override def parse(
      inputResult: ParseResult[String, String]): ParseResult[String, NestedObject] = {
    val columnsResult = inputResult.transform { line: String =>
      // Split the TSV line.
      val cols = line.split("\t")
      cols match {
        case Array(a, b, c) => TransformSuccess((a, b, c))
        case Array(a, b, c, _*) => TransformWarning((a, b, c),
          TooManyColumnsParseError(cols.length))
        case _ => TransformFailure(
          NotEnoughColumnsParseError(cols.length))
      }
    }

    // Split the components of the [[Ratio]] object and parse the numbers.
    columnsResult.transform { case (a, b, cRaw) =>
      val ratioSplits = cRaw.split(":")
      ratioSplits match {
        case Array(xRaw, yRaw) => Try(Ratio(xRaw.toInt, yRaw.toInt)) match {
          case Success(c) => TransformSuccess(NestedObject(a, b, c))
          case Failure(e: NumberFormatException) => TransformFailure(InvalidNumbersParseError(e))
          case Failure(e) => throw new RuntimeException(e)
        }
      }
    }
  }
}

/** Companion of [[SampleNestedObjectParser]] which defines
  * [[com.avira.ds.sparser.ParseError]]s that be reported by the parser.
  */
object SampleNestedObjectParser {

  sealed abstract class SampleNestedObjectParseError(
      override val message: Option[String],
      override val args: Seq[Any]) extends ParseError

  case class TooManyColumnsParseError(colsCount: Int)
      extends SampleNestedObjectParseError(Some("Too many columns"), Seq(colsCount))

  case class NotEnoughColumnsParseError(colsCount: Int)
      extends SampleNestedObjectParseError(Some("Insufficient columns"), Seq(colsCount))

  case class InvalidNumbersParseError(e: NumberFormatException)
      extends SampleNestedObjectParseError(
        Some("At least one of the numbers in Ratio is invalid"), Seq(e))

  /** Returns all [[ParseError]]s which could be reported by
    * [[SampleNestedObjectParser]].
    */
  def parseErrorClasses: Set[Class[_ <: ParseError]] =
    MacroUtils.getSealedClassChildren[SampleNestedObjectParseError]
        .asInstanceOf[Set[Class[_ <: ParseError]]]
}
