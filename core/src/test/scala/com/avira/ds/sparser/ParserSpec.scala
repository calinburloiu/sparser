/*
 * Copyright 2015 Avira Operations GmbH
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package com.avira.ds.sparser

import com.avira.ds.sparser.samples.{SamplePerson, SamplePersonParser}
import SamplePersonParser.{NotEnoughColumnsParseError, TooManyColumnsParseError, InvalidAgeParseError}
import org.scalatest.WordSpec

case class MyParseError(override val message: Option[String]) extends ParseError {
  override val args: Seq[Any] = Seq()
}

/** Integration test for a [[Parser]] sample implementation, [[SamplePersonParser]] */
class ParserSpec extends WordSpec {

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
