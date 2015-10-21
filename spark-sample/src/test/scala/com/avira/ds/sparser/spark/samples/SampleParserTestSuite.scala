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
package com.avira.ds.sparser.spark.samples

import com.avira.ds.sparser.samples.{SamplePerson, SamplePersonParser}
import com.avira.ds.sparser.test._
import com.avira.ds.sparser.{Parser, ParserConf}

/** Sample usage of `ParserTestSuite` on a sample parser */
class SampleParserTestSuite
    extends ParserTestSuite[String, SamplePerson] {

  override lazy val parser: Parser[String, SamplePerson] = new SamplePersonParser(ParserConf())

  ParserTest("Good input",
    "John Doe\t33",
    ExpectedSuccessResult(
      ExpectedValue(
        FieldMatch(_.name, "John Doe"),
        FieldMatch(_.age, 33)
      )
    )
  )

  ParserTest("Bad input",
    "Jon Snow",
    ExpectedFailureResult(
      ExpectedErrors(
        classOf[SamplePersonParser.NotEnoughColumnsParseError]
      )
    )
  )
}
