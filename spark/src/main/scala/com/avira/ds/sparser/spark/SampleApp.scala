package com.avira.ds.sparser.spark

import com.avira.ds.sparser._
import com.avira.ds.sparser.samples.{SamplePerson, SamplePersonParser}
import com.typesafe.scalalogging.slf4j.StrictLogging
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.{SparkConf, SparkContext}

import scala.util.Try

object SampleApp extends StrictLogging {

  def main(args: Array[String]): Unit = {
    val opts = Options(
      inputPath = args(0),
      shouldCollectInput = Try(args(1).toBoolean).getOrElse(true)
    )

    lazy val conf = new SparkConf()
    lazy val sc = new SparkContext(conf)

    val parserAccumulators = new ParserAccumulators(sc, SamplePersonParser.parseErrorClasses)

    val parserConf: ParserConf = ParserConf(
      errorCallback = parserAccumulators.createAccumulatorsParserCallback,
      shouldCollectInput = opts.shouldCollectInput
    )
    implicit val parserGenerator: () => Parser[String, SamplePerson] = { () =>
      new SamplePersonParser(parserConf)
    }
    implicit val parser: Parser[String, SamplePerson] = new SamplePersonParser(parserConf)
    implicit val broadcastParser: Broadcast[Parser[String, SamplePerson]] = sc.broadcast(parser)

    runWithoutErrors(sc, opts.inputPath)
    runWithGroupedErrors(sc, opts.inputPath)
    runWithExplodedErrors(sc, opts.inputPath)
    printErrorsReport(parserAccumulators)
  }

  def runWithoutErrors(
      sc: SparkContext,
      inputPath: String)(implicit parserGenerator: () => Parser[String, SamplePerson]): Unit = {
    import com.avira.ds.sparser.spark.SparserSpark.ParserGeneratorRDDFunctions

    val input = sc.textFile(inputPath)
    val persons = input.parse

    val l_persons = persons.collect()

    logger.info("*** Parsed values without errors:")
    l_persons.foreach(x => logger.info(x.toString))
  }

  def runWithGroupedErrors(
      sc: SparkContext,
      inputPath: String)(implicit parser: Parser[String, SamplePerson]): Unit = {
    import com.avira.ds.sparser.spark.SparserSpark.ParserRDDFunctions

    val input = sc.textFile(inputPath)
    val results = input.parseWithErrors

    val groupedErrors = results.flatMap {
      case ParseResult.Warning(_, warns, line) => Some((line, warns))
      case ParseResult.Failure(errs, line) => Some((line, errs))
      case _ => None
    }

    val l_groupedErrors = groupedErrors.collect()

    logger.warn("*** Grouped errors:")
    l_groupedErrors.foreach(x => logger.warn(x.toString()))
  }

  def runWithExplodedErrors(
      sc: SparkContext,
      inputPath: String)
      (implicit parserBroadcast: Broadcast[Parser[String, SamplePerson]]): Unit = {
    import com.avira.ds.sparser.spark.SparserSpark.ParserBroadcastRDDFunctions

    val input = sc.textFile(inputPath)
    val results = input.parseWithErrors

    val explodedErrors = for {
      result <- results
      err <- result.errors
      input <- result.input
    } yield (err, input)

    val l_explodedErrors = explodedErrors.collect()

    logger.warn("*** Exploded errors:")
    l_explodedErrors.foreach(x => logger.warn(x.toString()))
  }

  def printErrorsReport(parserAccumulators: ParserAccumulators): Unit = {
    logger.info("*** Errors report:")

    parserAccumulators.accumulators.foreach { case (name, acc) =>
      logger.info(s"\t$name=${acc.value}")
    }
  }
}

case class Options(
    inputPath: String,
    shouldCollectInput: Boolean) extends Serializable