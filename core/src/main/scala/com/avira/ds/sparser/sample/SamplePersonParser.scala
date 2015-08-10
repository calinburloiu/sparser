package com.avira.ds.sparser.sample

import com.avira.ds.MacroUtils
import com.avira.ds.sparser._
import com.avira.ds.sparser.sample.SamplePersonParser.{NotEnoughColumnsParseError, TooManyColumnsParseError, InvalidAgeParseError}

import scala.util.{Failure, Success, Try}

case class SamplePerson(
    name: String,
    age: Int)

class SamplePersonParser(override val conf: ParserConf)
    extends Parser[String, SamplePerson] {

  override def parse(input: String): ParseResult[String, SamplePerson] = {
    val splitResult: ParseResult[String, (String, String)] = createResult(input, input)
        .transform { line: String =>
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

  def parseErrorClasses: Set[Class[_ <: ParseError]] =
    MacroUtils.getSealedClassChildren[SamplePersonParseError]
       .asInstanceOf[Set[Class[_ <: ParseError]]]
}
