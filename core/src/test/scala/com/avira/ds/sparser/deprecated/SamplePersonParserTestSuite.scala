package com.avira.ds.sparser.deprecated

import com.avira.ds.sparser.{Parser, ParserConf}
import com.avira.ds.sparser.samples.{SamplePerson, SamplePersonParser}

@deprecated
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
