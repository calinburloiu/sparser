package com.avira.ds.parsing.sample

import com.avira.ds.parsing._

import scala.util.{Failure, Success, Try}

case class SamplePerson(
    name: String,
    age: Int)

class SamplePersonParser(override val conf: ParserConf)
    extends Parser[String, SamplePerson] {

  private var _errorsCount: Long = 0L

  override def parse(input: String): ParseResult[String, SamplePerson] = {
    val splitResult: ParseResult[String, (String, String)] = createResult(input, input)
        .transform { line: String =>
      val cols = line.split("\t")
      cols.length match {
        case x: Int if x < 2 =>
          TransformFailure(
            ParseError("columns.notEnough", Some("Insufficient columns"), cols.length)
          )
        case x: Int if x > 2 =>
          val (rawName, rawAge) = (cols(0), cols(1))
          TransformWarning(
            (rawName, rawAge),
            ParseError("columns.tooMany", Some("Too many columns"), cols.length)
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
        case Failure(e) =>
          TransformWarning(
            SamplePerson(rawName, -1),
            ParseError("age.invalid", Some(s"Invalid age $rawAge"), e)
          )
      }
    }
  }

}
