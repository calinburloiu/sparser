package com.avira.ds.parsing.sample

import com.avira.ds.parsing._

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
            ParseError("columns.tooMany", Some("Too many columns"), cols.length))
        case _ => TransformFailure(
            ParseError("columns.notEnough", Some("Insufficient columns"), cols.length))
      }
    }

    columnsResult.transform { case (a, b, cRaw) =>
      val ratioSplits = cRaw.split(":")
      ratioSplits match {
        case Array(xRaw, yRaw) => Try(Ratio(xRaw.toInt, yRaw.toInt)) match {
          case Failure(e) => TransformFailure(ParseError("numbers.invalid",
              Some("At least one of the numbers in Ratio is invalid"), e))
          case Success(c) => TransformSuccess(NestedObject(a, b, c))
        }
      }
    }
  }
}
