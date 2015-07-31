package com.avira.ds

import com.avira.ds.parsing._
import com.avira.ds.parsing.sample.SamplePersonParser
import com.typesafe.scalalogging.slf4j.StrictLogging
import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.SparkContext._

import scala.util.Try

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
    val sc = new SparkContext(conf)

    val inputPath = args(0)
    val shouldCollectInput = Try(args(1).toBoolean).getOrElse(true)
    val shouldCollectErrorMessages = Try(args(2).toBoolean).getOrElse(true)
    val shouldCollectErrorArgs = Try(args(3).toBoolean).getOrElse(true)

    val errorsAcc = sc.accumulator(0L, "errors")
    val parserConf: ParserConf = ParserConf(
      errorCallback = { err: ParseError =>
        errorsAcc += 1
      },
      shouldCollectInput = shouldCollectInput,
      shouldCollectErrorMessages = shouldCollectErrorMessages,
      shouldCollectErrorArgs = shouldCollectErrorArgs
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

    logger.info(s"Parsing finished with ${errorsAcc.value} errors.")
    sc.stop()

    logger.info("Parsed:")
    l_persons.foreach(x => logger.info(x.toString))
    logger.warn("Grouped errors:")
    l_errors.foreach(x => logger.warn(x.toString()))
    logger.warn("Exploded errors:")
    l_errorsWithInput.foreach(x => logger.warn(x.toString()))

  }
}
