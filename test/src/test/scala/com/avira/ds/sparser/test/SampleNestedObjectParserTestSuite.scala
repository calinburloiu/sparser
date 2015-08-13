package com.avira.ds.sparser.test

import com.avira.ds.sparser.Parser
import com.avira.ds.sparser.samples.{SampleNestedObjectParser, NestedObject}

class SampleNestedObjectParserTestSuite extends ParserTestSuite[String, NestedObject] {

  override lazy val parser: Parser[String, NestedObject] = new SampleNestedObjectParser

  ParserTest("Good",
    input="Calin\tAndrei\t5:4",
    ExpectedSuccessResult(
      ExpectedValue(
        FieldMatch(_.a, "Calin"),
        FieldMatch(_.b, "Andrei"),
        FieldMatch(_.c.x, 5),
        FieldMatch(_.c.y, 4)
      )
    )
  )
}
