package com.avira.ds.sparser.sample

import com.avira.ds.sparser._

class SamplePersonParserTestSuite extends ParserTestSuite[String, SamplePerson] {
  override lazy val parser: Parser[String, SamplePerson] = new SamplePersonParser(ParserConf())

  ParserTest("Good",
    "Calin\t28",
    ExpectedValue(
      FieldMatch(_.name, "Calin"),
      FieldMatch(_.age, 28)
    )
  )
}
