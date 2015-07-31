package com.avira.ds.parsing

import com.avira.ds.parsing.sample.{SamplePerson, SamplePersonParser}
import org.scalatest.WordSpec

import scala.collection.mutable

class ParserSpec extends WordSpec {

  implicit val conf = ParserConf()

  "A ParserResult" when {
    "it has no errors" should {
      val result = ParseResult.Success("some value")

      "have 0 errors" in {
        assert(result.errors.isEmpty)
      }

      "return a result with 1 error when an error is reported" in {
        val newResult = result.reportError(ParseError("some error", None))
        assert(newResult.errors.size == 1)
      }
    }

    "it has an error" should {
      val result = ParseResult.Warning("some value", Seq(ParseError("some error", None)))
      val newResult = result.reportError(ParseError("some other error", None))

      "return a result with 2 errors when a error is reported" in {
        assert(newResult.errors.size == 2)
        assert(newResult.errors.map(_.name) == Seq("some error", "some other error"))
      }
    }

//    "it has an error callback which adds the error to a list" should {
//      val errorList = new mutable.ArrayBuffer[ParseError]
//      val callback: (ParseError => Unit) = { error: ParseError =>
//        errorList += error
//      }
//      val result = ParseResult[Nothing](callback)
//
//      "have the side effect of adding the error in the list when an error is reported" in {
//        result.reportError(ParseError("some error", None))
//        println(errorList)
//        assert(errorList.size == 1)
//      }
//    }

    "it has a value and transform is called" should {
      val result = ParseResult.Warning("Hello, world!", Seq(ParseError("first")))

      "return a result with a new value and the old error on success" in {
        val newResult = result.transform { value =>
          TransformSuccess(value.length)
        }

        assert(newResult.get == 13)
        assert(newResult.errors.size == 1)
      }

      "return a result with a new value and a new error besides the old one on warning" in {
        val newResult = result.transform { value =>
          TransformWarning(value.length, ParseError("warning"))
        }

        assert(newResult.get == 13)
        assert(newResult.errors.size == 2)
      }

      "return a result with no value and a new error besides the old one on failure" in {
        val newResult = result.transform { value =>
          TransformFailure(ParseError("failure"))
        }

        intercept[NoSuchElementException] {
          newResult.get
        }
        assert(newResult.errors.size == 2)
      }
    }

    "it doesn't have a value and transform is called" should {
      val emptyResult: ParseResult[String, String] =
        ParseResult.Failure[String](Seq(ParseError("error.first")))

      "return an equal result no matter what transform does" in {
        val successResult = emptyResult.transform { value =>
          TransformSuccess(value.length)
        }

        val warningResult = emptyResult.transform { value =>
          TransformWarning(value.length, ParseError("warning"))
        }

        val failureResult = emptyResult.transform { value =>
          TransformFailure(ParseError("failure"))
        }

        assert(successResult.asInstanceOf[ParseResult[Any, Any]] ==
            emptyResult.asInstanceOf[ParseResult[Any, Any]])
        assert(warningResult.asInstanceOf[ParseResult[Any, Any]] ==
            emptyResult.asInstanceOf[ParseResult[Any, Any]])
        assert(failureResult.asInstanceOf[ParseResult[Any, Any]] ==
            emptyResult.asInstanceOf[ParseResult[Any, Any]])
      }
    }
  }

  "A SamplePersonParser" when {
    "parsing a valid TSV line" should {
      var errorsCount = 0
      val parser = new SamplePersonParser(ParserConf(errorCallback = { error: ParseError =>
        errorsCount += 1
      }))
      val line = "Calin\t28"
      val result = parser.parse(line)

      "return a ParserResult with a SamplePerson value and no errors" in {
        assert(result.get == SamplePerson("Calin", 28))
        assert(result.errors.isEmpty)
      }

      "have no side effects and leave errorsCount unchanged" in {
        assert(errorsCount == 0)
      }
    }

    "parsing a TSV line with less columns than expected" should {
      var errorsCount = 0
      val parser = new SamplePersonParser(ParserConf(errorCallback = { error: ParseError =>
        errorsCount += 1
      }))
      val line = "Calin"
      val result = parser.parse(line)

      "return a ParserResult without value and error" in {
        intercept[NoSuchElementException] {
          result.get
        }
        assert(result.errors.size == 1)
      }

      "have side effects and increment errorsCount" in {
        assert(errorsCount == 1)
      }
    }

    "parsing a TSV line with more columns than expected and invalid age" should {
      var errorsCount = 0
      val parser = new SamplePersonParser(ParserConf(errorCallback = { error: ParseError =>
        errorsCount += 1
      }))
      val line = "Calin\t2o\tprogrammer"
      val result = parser.parse(line)

      "return a ParserResult with a SamplePerson value and errors" in {
        assert(result.get == SamplePerson("Calin", -1))
        assert(result.errors.size == 2)
      }

      "have side effects and increment errorsCount" in {
        assert(errorsCount == 2)
      }
    }
  }
}
