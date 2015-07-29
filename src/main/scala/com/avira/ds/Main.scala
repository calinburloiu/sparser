package com.avira.ds

import com.avira.ds.parsing._
import com.typesafe.scalalogging.slf4j.StrictLogging
import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.SparkContext._

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
    val sc = new SparkContext(conf)

    val inputPath = args(0)

    val errorsAcc = sc.accumulator(0L, "errors")
    val parserConf: ParserConf = ParserConf(
      errorCallback = { err: ParseError =>
        errorsAcc += 1
      },
      collectInput = true,
      collectErrorMessages = true,
      collectErrorArgs = true
    )
    val parser = new SamplePersonParser(parserConf)

    val input = sc.textFile(inputPath)

    val parseResults = input.map { line =>
      parser.parse(line)
    }

    val persons = parseResults.flatMap {
      case ParseResult.Success(value, _) => Some(value)
      case _ => None
    }

    val errors = parseResults.flatMap {
      case ParseResult.Warning(_, warns, _) => warns
      case ParseResult.Failure(errs, _) => errs
      case _ => Seq()
    }

    val errorsWithInput = for {
      result <- parseResults
      err <- result.errors
      input <- result.input
    } yield (err, input)

    val l_persons = persons.collect()
    val l_errors = errors.collect()
    val l_errorsWithInput = errorsWithInput.collect()

    logger.info(s"Parsing finished with ${errorsAcc.value} errors.")
    sc.stop()

    logger.info("Parsed:")
    l_persons.foreach(x => logger.info(x.toString))
    logger.warn("Errors:")
    l_errors.foreach(x => logger.warn(x.toString))
    logger.warn("Errors with input:")
    l_errorsWithInput.foreach(x => logger.warn(x.toString()))

  }
}
