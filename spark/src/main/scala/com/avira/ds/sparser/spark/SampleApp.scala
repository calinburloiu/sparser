package com.avira.ds.sparser.spark

import com.avira.ds.sparser._
import com.avira.ds.sparser.sample.SamplePersonParser
import com.avira.ds.sparser.spark.SparserSpark._
import com.typesafe.scalalogging.slf4j.{LazyLogging, StrictLogging}
import org.apache.spark.{SparkConf, SparkContext}

import scala.util.Try

object SampleApp extends StrictLogging {

  def main(args: Array[String]): Unit = {
    val opts = Options(
      inputPath = args(0),
      shouldCollectInput = Try(args(1).toBoolean).getOrElse(true),
      shouldCollectErrorMessages = Try(args(2).toBoolean).getOrElse(true),
      shouldCollectErrorArgs = Try(args(3).toBoolean).getOrElse(true)
    )

    lazy val conf = new SparkConf()
    lazy val sc = new SparkContext(conf)

    val parserAccumulators = ParserAccumulators(sc)

    val parserConf: ParserConf = ParserConf(
      errorCallback = parserAccumulators.createAccumulatorsParserCallback,
      shouldCollectInput = opts.shouldCollectInput,
      shouldCollectErrorMessages = opts.shouldCollectErrorMessages,
      shouldCollectErrorArgs = opts.shouldCollectErrorArgs
    )
    implicit val parser = new SamplePersonParser(parserConf)
    implicit val parserGenerator = { () => new SamplePersonParser(parserConf) }

    runWithoutErrors(sc, opts.inputPath)
    runWithGroupedErrors(sc, opts.inputPath)
    runWithExplodedErrors(sc, opts.inputPath)
    printErrorsReport(parserAccumulators)
  }

  def runWithoutErrors(
      sc: SparkContext,
      inputPath: String)(implicit parserGenerator: () => SamplePersonParser): Unit = {
    implicit val impl = parserGenerator()

    val input = sc.textFile(inputPath)
    val persons = input.parse

    val l_persons = persons.collect()

    logger.info("Parsed values without errors:")
    l_persons.foreach(x => logger.info(x.toString))
  }

  def runWithGroupedErrors(
      sc: SparkContext,
      inputPath: String)(implicit parserGenerator: () => SamplePersonParser): Unit = {
    val input = sc.textFile(inputPath)
    val results = input.parseWithErrors

    val groupedErrors = results.flatMap {
      case ParseResult.Warning(_, warns, line) => Some((line, warns))
      case ParseResult.Failure(errs, line) => Some((line, errs))
      case _ => None
    }

    val l_groupedErrors = groupedErrors.collect()

    logger.warn("Grouped errors:")
    l_groupedErrors.foreach(x => logger.warn(x.toString()))
  }

  def runWithExplodedErrors(
      sc: SparkContext,
      inputPath: String)(implicit parserGenerator: () => SamplePersonParser): Unit = {
    val input = sc.textFile(inputPath)
    val results = input.parseWithErrors

    val explodedErrors = for {
      result <- results
      err <- result.errors
      input <- result.input
    } yield (err, input)

    val l_explodedErrors = explodedErrors.collect()

    logger.warn("Exploded errors:")
    l_explodedErrors.foreach(x => logger.warn(x.toString()))
  }

  def printErrorsReport(parserAccumulators: ParserAccumulators): Unit = {
    logger.info("Errors report:")

    parserAccumulators.accumulators.foreach { acc =>
      logger.info(s"\t${acc.name.get}=${acc.value}")
    }
  }
}

case class Options(
    inputPath: String,
    shouldCollectInput: Boolean,
    shouldCollectErrorMessages: Boolean,
    shouldCollectErrorArgs: Boolean) extends Serializable
