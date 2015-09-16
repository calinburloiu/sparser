package com.avira.ds.sparser.test

import com.avira.ds.sparser.Parser
import com.avira.ds.sparser.samples.{Ratio, SampleNestedObjectParser, NestedObject}

/** Test suite example for [[com.avira.ds.sparser.samples.SampleNestedObjectParser]]. */
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

  ParserTest[Ratio]("partial function goes deep",
    input="Calin\tAndrei\t5:4",
    { case NestedObject(_, _, c) => c },
    ExpectedSuccessResult(
      ExpectedValue(
        FieldMatch(_.x, 5),
        FieldMatch(_.y, 4)
      )
    )
  )
}
