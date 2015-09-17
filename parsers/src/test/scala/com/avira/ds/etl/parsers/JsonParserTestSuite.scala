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
