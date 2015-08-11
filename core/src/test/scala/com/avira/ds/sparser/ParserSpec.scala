package com.avira.ds.sparser

import com.avira.ds.sparser.samples.SamplePersonParser.{NotEnoughColumnsParseError, TooManyColumnsParseError, InvalidAgeParseError}
import com.avira.ds.sparser.samples.{SamplePerson, SamplePersonParser}
import org.scalatest.WordSpec

import scala.collection.mutable

case class MyParseError(override val message: Option[String]) extends ParseError {
  override val args: Seq[Any] = Seq()
}

class ParserSpec extends WordSpec {

  implicit val conf = ParserConf()

  "A ParserResult" when {
    "it has no errors" should {
      val result = ParseResult.Success("some value")

      "have 0 errors" in {
        assert(result.errors.isEmpty)
      }

      "return a result with 1 error when an error is reported" in {
        val newResult = result.reportError(MyParseError(None))
        assert(newResult.errors.size == 1)
      }
    }

    "it has an error" should {
      val result = ParseResult.Warning("some value", Seq(MyParseError(Some("some error"))))
      val newResult = result.reportError(MyParseError(Some("some other error")))

      "return a result with 2 errors when a error is reported" in {
        assert(newResult.errors.size == 2)
        assert(newResult.errors.flatMap(_.message) == Seq("some error", "some other error"))
      }
    }

    "it has an error callback which adds the error to a list" should {
      val errorList = new mutable.ArrayBuffer[ParseError]
      val parserConf = ParserConf({ error: ParseError =>
        errorList += error
      })
      val result = ParseResult.Success("value")(parserConf)

      "have the side effect of adding the error in the list when an error is reported" in {
        result.reportError(MyParseError(Some("some error")))
        assert(errorList.size == 1)
      }
    }

    "it has a value and transform is called" should {
      val result = ParseResult.Warning("Hello, world!", Seq(MyParseError(None)))

      "return a result with a new value and the old error on success" in {
        val newResult = result.transform { value =>
          TransformSuccess(value.length)
        }

        assert(newResult.get == 13)
        assert(newResult.errors.size == 1)
      }

      "return a result with a new value and a new error besides the old one on warning" in {
        val newResult = result.transform { value =>
          TransformWarning(value.length, MyParseError(None))
        }

        assert(newResult.get == 13)
        assert(newResult.errors.size == 2)
      }

      "return a result with no value and a new error besides the old one on failure" in {
        val newResult = result.transform { value =>
          TransformFailure(MyParseError(None))
        }

        intercept[NoSuchElementException] {
          newResult.get
        }
        assert(newResult.errors.size == 2)
      }
    }

    "it doesn't have a value and transform is called" should {
      val emptyResult: ParseResult[String, String] =
        ParseResult.Failure[String](Seq(MyParseError(None)))

      "return an equal result no matter what transform does" in {
        val successResult = emptyResult.transform { value =>
          TransformSuccess(value.length)
        }

        val warningResult = emptyResult.transform { value =>
          TransformWarning(value.length, MyParseError(None))
        }

        val failureResult = emptyResult.transform { value =>
          TransformFailure(MyParseError(None))
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

  "Calling SamplePersonParser.parseErrorClasses" should {
    "return a list of all ParseError classes that correspond to SamplePersonParser errors" in {
      val classes = SamplePersonParser.parseErrorClasses
      assert(classes contains classOf[InvalidAgeParseError])
      assert(classes contains classOf[TooManyColumnsParseError])
      assert(classes contains classOf[NotEnoughColumnsParseError])
    }
  }
}
