package com.avira.ds.parsing

import com.avira.ds.parsing.ParserResult.{TransformFailure, TransformWarning, TransformSuccess}
import org.scalatest.WordSpec

import scala.collection.mutable

class ParserSpec extends WordSpec {

  "A ParserResult" when {
    "it has no errors" should {
      val result = ParserResult("some value")

      "have 0 errors" in {
        assert(result.errors.isEmpty)
      }

      "return a result with 1 error when an error is reported" in {
        val newResult = result.reportError(ParserError("some error", None))
        assert(newResult.errors.size == 1)
      }
    }

    "it has an error" should {
      val result = ParserResult("some value", ParserError("some error", None))
      val newResult = result.reportError(ParserError("some other error", None))

      "return a result with 2 errors when a error is reported" in {
        assert(newResult.errors.size == 2)
        assert(newResult.errors.map(_.name) == Seq("some error", "some other error"))
      }
    }

    "it has an error callback which adds the error to a list" should {
      val errorList = new mutable.ArrayBuffer[ParserError]
      val callback: (ParserError => Unit) = { error: ParserError =>
        errorList += error
      }
      val result = ParserResult[Nothing](callback)

      "have the side effect of adding the error in the list when an error is reported" in {
        result.reportError(ParserError("some error", None))
        println(errorList)
        assert(errorList.size == 1)
      }
    }

    "it has a value and transform is called" should {
      val result = ParserResult("Hello, world!", ParserError("first"))

      "return a result with a new value and the old error on success" in {
        val newResult = result.transform { value =>
          TransformSuccess(value.length)
        }

        assert(newResult.value.get == 13)
        assert(newResult.errors.size == 1)
      }

      "return a result with a new value and a new error besides the old one on warning" in {
        val newResult = result.transform { value =>
          TransformWarning(value.length, ParserError("warning"))
        }

        assert(newResult.value.get == 13)
        assert(newResult.errors.size == 2)
      }

      "return a result with no value and a new error besides the old one on failure" in {
        val newResult = result.transform { value =>
          TransformFailure(ParserError("failure"))
        }

        assert(newResult.value.isEmpty)
        assert(newResult.errors.size == 2)
      }
    }

    "it doesn't have a value and transform is called" should {
      val emptyResult = ParserResult[String](None, Seq(ParserError("error.first")))

      "return an equal result no matter what transform does" in {
        val successResult = emptyResult.transform { value =>
          TransformSuccess(value.length)
        }

        val warningResult = emptyResult.transform { value =>
          TransformWarning(value.length, ParserError("warning"))
        }

        val failureResult = emptyResult.transform { value =>
          TransformFailure(ParserError("failure"))
        }

        assert(successResult.asInstanceOf[ParserResult[Any]] ==
            emptyResult.asInstanceOf[ParserResult[Any]])
        assert(warningResult.asInstanceOf[ParserResult[Any]] ==
            emptyResult.asInstanceOf[ParserResult[Any]])
        assert(failureResult.asInstanceOf[ParserResult[Any]] ==
            emptyResult.asInstanceOf[ParserResult[Any]])
      }
    }
  }

  "A SamplePersonParser" when {
    "parsing a valid TSV line" should {
      val parser = new SamplePersonParser
      val line = "Calin\t28"
      parser.resetErrorsCount()
      val result = parser.parse(line)

      "return a ParserResult with a SamplePerson value and no errors" in {
        assert(result.value.get == SamplePerson("Calin", 28))
        assert(result.errors.isEmpty)
      }

      "have no side effects and leave errorsCount unchanged" in {
        assert(parser.errorsCount == 0)
      }
    }

    "parsing a TSV line with less columns than expected" should {
      val parser = new SamplePersonParser
      val line = "Calin"
      parser.resetErrorsCount()
      val result = parser.parse(line)

      "return a ParserResult without value and error" in {
        assert(result.value.isEmpty)
        assert(result.errors.size == 1)
      }

      "have side effects and increment errorsCount" in {
        assert(parser.errorsCount == 1)
      }
    }

    "parsing a TSV line with more columns than expected and invalid age" should {
      val parser = new SamplePersonParser
      val line = "Calin\t2o\tprogrammer"
      parser.resetErrorsCount()
      val result = parser.parse(line)

      "return a ParserResult with a SamplePerson value and errors" in {
        assert(result.value.get == SamplePerson("Calin", -1))
        assert(result.errors.size == 2)
      }

      "have side effects and increment errorsCount" in {
        assert(parser.errorsCount == 2)
      }
    }
  }
}
