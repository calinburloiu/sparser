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
