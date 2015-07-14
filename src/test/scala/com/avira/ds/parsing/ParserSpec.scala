package com.avira.ds.parsing

import org.scalatest.WordSpec

import scala.collection.mutable

class ParserSpec extends WordSpec {

  "A ParserResult" when {
    "it has no errors" should {
      val emptyResult = ParserResult("some value")

      "have 0 errors" in {
        assert(emptyResult.errors.isEmpty)
      }

      "have 1 error when an error is reported" in {
        val newResult = emptyResult.reportError(ParserError("some error", None))
        assert(newResult.errors.size == 1)
      }
    }

    "it has an error" should {
      val result = ParserResult("some value", ParserError("some error", None))
      val newResult = result.reportError(ParserError("some other error", None))

      "have 2 errors when a new error is reported" in {
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

      "have 1 error in the list when an error is reported" in {
        result.reportError(ParserError("some error", None))
        println(errorList)
        assert(errorList.size == 1)
      }
    }
  }
}
