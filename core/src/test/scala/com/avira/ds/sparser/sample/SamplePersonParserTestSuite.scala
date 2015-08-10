package com.avira.ds.sparser.sample

import com.avira.ds.sparser._
import com.avira.ds.sparser.sample.SamplePersonParser.SamplePersonParseError

class SamplePersonParserTestSuite
    extends ParserTestSuite[String, SamplePerson] {

  override lazy val parser: Parser[String, SamplePerson] = new SamplePersonParser(ParserConf())

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
