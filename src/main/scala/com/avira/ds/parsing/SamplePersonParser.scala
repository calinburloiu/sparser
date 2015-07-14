package com.avira.ds.parsing

import scala.util.{Failure, Success, Try}

case class SamplePerson(
    name: String,
    age: Int)

class SamplePersonParser1 extends Parser[SamplePerson] {

  override def parse(line: String): ParserResult[SamplePerson] = {
    createResult(line).pipe[(String, String)] { lineOpt: Option[String] =>
      // FIXME unsafe get
      val line = lineOpt.get

      val cols = line.split("\t")
      cols.length match {
        case x: Int if x < 2 => (None,
            Some(ParserError("columns.notEnough", Some("Insufficient columns"), cols.length)))
        case x: Int if x > 2 => (None,
            Some(ParserError("columns.tooMany", Some("Too many columns"), cols.length)))
        case x: Int =>
          val Array(rawName, rawAge) = cols
          (Some((rawName, rawAge)), None)
      }
    }.pipe[SamplePerson] {
      case None => (None, None)
      case Some((rawName, rawAge)) => Try(rawAge.toInt) match {
        case Success(age) if age >= 0 => (Some(SamplePerson(rawName, age)), None)
        case Failure(e) => (Some(SamplePerson(rawName, -1)),
            Some(ParserError("age.invalid", Some(s"Invalid age $rawAge"), e)))
      }
    }
  }
}

class SamplePersonParser2 extends Parser[SamplePerson] {

  override def parse(line: String): ParserResult[SamplePerson] = {
    val splitResult = createResult(line).pipe2[(String, String)] { line: String =>
      val cols = line.split("\t")
      cols.length match {
        case x: Int if x < 2 =>
          PipeFailure(
            ParserError("columns.notEnough", Some("Insufficient columns"), cols.length)
          )
        case x: Int if x > 2 =>
          PipeFailure(
            ParserError("columns.tooMany", Some("Too many columns"), cols.length)
          )
        case x: Int =>
          val Array(rawName, rawAge) = cols
          PipeSuccess(
            (rawName, rawAge)
          )
      }
    }

    splitResult.pipe2[SamplePerson] {
      case (rawName, rawAge) => Try(rawAge.toInt) match {
        case Success(age) if age >= 0 =>
          PipeSuccess(
            SamplePerson(rawName, age)
          )
        case Failure(e) =>
          PipeWarning(
            SamplePerson(rawName, -1),
            ParserError("age.invalid", Some(s"Invalid age $rawAge"), e)
          )
      }
    }
  }
}
