package com.avira.ds.parsing

import scala.util.{Failure, Success, Try}

case class SamplePerson(
    name: String,
    age: Int)

class SamplePersonParser extends Parser[String, SamplePerson] {

  import com.avira.ds.parsing.ParserResult.{TransformSuccess, TransformWarning, TransformFailure}

  override def parse(line: String): ParserResult[SamplePerson] = {
    val splitResult = createResult(line).transform { line: String =>
      val cols = line.split("\t")
      cols.length match {
        case x: Int if x < 2 =>
          TransformFailure(
            ParserError("columns.notEnough", Some("Insufficient columns"), cols.length)
          )
        case x: Int if x > 2 =>
          val (rawName, rawAge) = (cols(0), cols(1))
          TransformWarning(
            (rawName, rawAge),
            ParserError("columns.tooMany", Some("Too many columns"), cols.length)
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
            ParserError("age.invalid", Some(s"Invalid age $rawAge"), e)
          )
      }
    }
  }
}
