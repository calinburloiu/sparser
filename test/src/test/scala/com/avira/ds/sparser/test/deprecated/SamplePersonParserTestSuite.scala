package com.avira.ds.sparser.test.deprecated

import com.avira.ds.sparser.samples.{SamplePerson, SamplePersonParser}
import com.avira.ds.sparser.{Parser, ParserConf}

@deprecated("Use the new com.avira.ds.sparser.test.ParserTestSuite", "0.1.0-SNAPSHOT")
class SamplePersonParserTestSuite extends ParserTestSuite[String, SamplePerson] {

  lazy val parser: Parser[String, SamplePerson] = new SamplePersonParser(ParserConf())

  lazy val tests = Seq(
    ParserTest("Good",
      "Calin\t28",
      ExpectedValue(
        "name" -> "Calin",
        "age" -> 28
      )
    ),
    ParserTest("Bad age",
      "Andrei\t2o",
      ExpectedValue(
        "name" -> "Andrei",
        "age" -> -1
      ),
      ExpectedErrors(
        classOf[SamplePersonParser.InvalidAgeParseError]
      )
    ),
    ParserTest("Bad input",
      "Burloiu",
      ExpectedNoValue,
      ExpectedErrors(
        classOf[SamplePersonParser.NotEnoughColumnsParseError]
      )
    )
  )
}
