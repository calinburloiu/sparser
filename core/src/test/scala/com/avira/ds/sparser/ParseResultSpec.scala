package com.avira.ds.sparser

import org.scalatest.{PrivateMethodTester, WordSpec}

import scala.collection.mutable

/** Tests for [[ParseResult]] class and its descendants */
class ParseResultSpec  extends WordSpec with PrivateMethodTester {

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

}
