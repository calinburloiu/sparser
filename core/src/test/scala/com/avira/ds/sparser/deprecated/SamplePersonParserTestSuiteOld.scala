package com.avira.ds.sparser.deprecated

import com.avira.ds.sparser.{Parser, ParserConf}
import com.avira.ds.sparser.deprecated._
import com.avira.ds.sparser.sample.{SamplePerson, SamplePersonParser}

class SamplePersonParserTestSuiteOld extends ParserTestSuiteOld[String, SamplePerson] {

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
        "age.invalid"
      )
    ),
    ParserTest("Bad input",
      "Burloiu",
      NoExpectedValue,
      ExpectedErrors(
        "columns.notEnough"
      )
    )
  )
}
