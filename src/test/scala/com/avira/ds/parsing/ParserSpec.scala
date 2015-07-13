package com.avira.ds.parsing

import org.scalatest.WordSpec

class ParserSpec extends WordSpec {

  "A ParserError" when {
    "empty" should {
      "have 0 messages" in {
        assert(ParserError().messages.isEmpty)
      }

      "have 1 message when appended" in {
        assert((ParserError() :+ "right").messages.size == 1)
      }

      "have 1 message when prepended" in {
        assert(("left" +: ParserError()).messages.size == 1)
      }
    }

    "non-empty" should {
      val line = Some("line")
      val init = ParserError(Seq("message"), line)

      "have an element at the right when appended" in {
        assert(init :+ "right" == ParserError(Seq("message", "right"), line))
      }

      "have an element at the left when prepended" in {
        assert("left" +: init == ParserError(Seq("left", "message"), line))
      }
    }
  }
}
