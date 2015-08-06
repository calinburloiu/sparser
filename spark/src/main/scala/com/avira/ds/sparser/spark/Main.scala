package com.avira.ds.sparser.spark

import com.avira.ds.sparser._
import com.avira.ds.sparser.sample.SamplePersonParser
import com.avira.ds.sparser.spark.SparserSpark._
import com.typesafe.scalalogging.slf4j.StrictLogging
import org.apache.spark.{SparkConf, SparkContext}

import scala.util.Try

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
    val sc = new SparkContext(conf)

    val inputPath = args(0)
    val shouldCollectInput = Try(args(1).toBoolean).getOrElse(true)
    val shouldCollectErrorMessages = Try(args(2).toBoolean).getOrElse(true)
    val shouldCollectErrorArgs = Try(args(3).toBoolean).getOrElse(true)

    val notEnoughColsAcc = sc.accumulator(0L, "columns.notEnough")
    val tooManyColsAcc = sc.accumulator(0L, "columns.tooMany")
    val invalidAgeAcc = sc.accumulator(0L, "age.invalid")
    val accumulators = Seq(notEnoughColsAcc, tooManyColsAcc, invalidAgeAcc)

    val parserConf: ParserConf = ParserConf(
      errorCallback = createAccumulatorsParserCallback(accumulators),
      shouldCollectInput = shouldCollectInput,
      shouldCollectErrorMessages = shouldCollectErrorMessages,
      shouldCollectErrorArgs = shouldCollectErrorArgs
    )
    implicit val parser = new SamplePersonParser(parserConf)

    val input = sc.textFile(inputPath)

    val parseResults = input.parseWithErrors
    val persons = input.parse

    val groupedErrors = parseResults.flatMap {
      case ParseResult.Warning(_, warns, line) => Some((line, warns))
      case ParseResult.Failure(errs, line) => Some((line, errs))
      case _ => None
    }

    val explodedErrors = for {
      result <- parseResults
      err <- result.errors
      input <- result.input
    } yield (err, input)

    val l_persons = persons.collect()
    val l_errors = groupedErrors.collect()
    val l_errorsWithInput = explodedErrors.collect()

    logger.info(s"Accumulators:")
    accumulators.foreach { acc =>
      logger.info(s"  ${acc.name.get}=${acc.value}")
    }
    sc.stop()

    logger.info("Parsed:")
    l_persons.foreach(x => logger.info(x.toString))
    logger.warn("Grouped errors:")
    l_errors.foreach(x => logger.warn(x.toString()))
    logger.warn("Exploded errors:")
    l_errorsWithInput.foreach(x => logger.warn(x.toString()))

  }
}
