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
package com.avira.ds.sparser.test

import com.avira.ds.sparser.samples.{SamplePersonParser, SamplePerson}
import com.avira.ds.sparser.{ParserConf, Parser}

/** Test suite example for [[com.avira.ds.sparser.samples.SamplePersonParser]]. */
class SamplePersonParserTestSuite
    extends ParserTestSuite[String, SamplePerson] {

  override val parser: Parser[String, SamplePerson] = new SamplePersonParser(ParserConf())

  ParserTest("Good",
    "Calin\t28",
    ExpectedSuccessResult(
      ExpectedValue(
        FieldMatch(_.name, "Calin"),
        FieldMatch(_.age, 28)
      )
    )
  )

  ParserTest("Good (only check if value exists)",
    "Calin\t28",
    ExpectedSuccessResult(
      ExpectedValue()
    )
  )

  ParserTest("Bad age",
    "Andrei\t2o",
    ExpectedWarningResult(
      ExpectedValue(
        FieldMatch(_.name, "Andrei"),
        FieldMatch(_.age, -1)
      ),
      ExpectedErrors(
        classOf[SamplePersonParser.InvalidAgeParseError]
      )
    )
  )

  ParserTest("Bad input",
    "Burloiu",
    ExpectedFailureResult(
      ExpectedErrors(
        classOf[SamplePersonParser.NotEnoughColumnsParseError]
      )
    )
  )
}
