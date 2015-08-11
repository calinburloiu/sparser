package com.avira.ds.sparser.samples

import com.avira.ds.MacroUtils
import com.avira.ds.sparser._
import com.avira.ds.sparser.samples.SampleNestedObjectParser.{InvalidNumbersParseError, NotEnoughColumnsParseError, TooManyColumnsParseError}

import scala.util.{Failure, Success, Try}

case class NestedObject(
    a: String,
    b: String,
    c: Ratio)

case class Ratio(
    x: Int,
    y: Int)

class SampleNestedObjectParser extends Parser[String, NestedObject] {

  override def parse(input: String): ParseResult[String, NestedObject] = {
    val inputResult = createResult(input, input)

    val columnsResult = inputResult.transform { line: String =>
      val cols = line.split("\t")
      cols match {
        case Array(a, b, c) => TransformSuccess((a, b, c))
        case Array(a, b, c, _*) => TransformWarning((a, b, c),
          TooManyColumnsParseError(cols.length))
        case _ => TransformFailure(
          NotEnoughColumnsParseError(cols.length))
      }
    }

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

  def parseErrorClasses: Set[Class[_ <: ParseError]] =
    MacroUtils.getSealedClassChildren[SampleNestedObjectParseError]
        .asInstanceOf[Set[Class[_ <: ParseError]]]
}
