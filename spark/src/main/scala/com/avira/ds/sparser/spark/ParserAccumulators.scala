package com.avira.ds.sparser.spark

import com.avira.ds.sparser.ParseError
import org.apache.spark.{Accumulator, SparkContext}

case class ParserAccumulators(sc: SparkContext) {
  val notEnoughColsAcc = sc.accumulator(0L, "columns.notEnough")
  val tooManyColsAcc = sc.accumulator(0L, "columns.tooMany")
  val invalidAgeAcc = sc.accumulator(0L, "age.invalid")
  val accumulators: Seq[Accumulator[Long]] = Seq(notEnoughColsAcc, tooManyColsAcc, invalidAgeAcc)

  // FIXME Should be "static".
  def createAccumulatorsParserCallback(
      accumulators: Map[String, Accumulator[Long]]): ParseError => Unit = { err: ParseError =>
    accumulators.get(err.name).foreach { acc =>
      acc += 1L
    }
  }

  def createAccumulatorsParserCallback: ParseError => Unit = {
    val accumulatorsMap = accumulators.map { acc =>
      if (acc.name.isEmpty) {
        throw new IllegalArgumentException("All accumulators need to be named")
      } else {
        acc.name.get -> acc
      }
    }.toMap

    createAccumulatorsParserCallback(accumulatorsMap)
  }
}
