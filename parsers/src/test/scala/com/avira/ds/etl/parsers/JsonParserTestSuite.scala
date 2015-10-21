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
package com.avira.ds.etl.parsers

import com.avira.ds.etl.parsers.JsonParser._
import com.avira.ds.sparser.samples.SamplePerson
import com.avira.ds.sparser.test._
import com.avira.ds.sparser.{Parser, ParserConf}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

/**
 * Test suite for [[JsonParser]].
 */
class JsonParserTestSuite extends ParserTestSuite[String, SamplePerson] {

//  implicit val personReads: Reads[SamplePerson] = (
//    (__ \ "name").read[String] and
//    (__ \ "age").read[Int]
//  )(SamplePerson.apply _)
  implicit val personReads: Reads[SamplePerson] = Json.reads[SamplePerson]
  override val parser: Parser[String, SamplePerson] = new JsonParser[SamplePerson](ParserConf())

  ParserTest("valid JSON",
    """{"name":"John","age":25}""",
    ExpectedSuccessResult(
      ExpectedValue(
        FieldMatch(_.name, "John"),
        FieldMatch(_.age, 25)
      )
    )
  )

  ParserTest("incomplete JSON",
    """{"name":"John"}""",
    ExpectedFailureResult(
      ExpectedErrors(
        classOf[InvalidJsonParseError]
      )
    )
  )

  ParserTest("invalid JSON",
    """{"name":"John""",
    ExpectedFailureResult(
      ExpectedErrors(
        classOf[MalformedJsonParseError]
      )
    )
  )

  ParserTest("invalid age",
    """{"name":"John","age":"25"}""",
    ExpectedFailureResult(
      ExpectedErrors(
        classOf[InvalidJsonParseError]
      )
    )
  )
}
