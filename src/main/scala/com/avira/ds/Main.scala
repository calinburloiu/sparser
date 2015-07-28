package com.avira.ds

import com.avira.ds.parsing.{ParseResult, SamplePersonParser}
import com.typesafe.scalalogging.slf4j.StrictLogging
import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.SparkContext._

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
    val sc = new SparkContext(conf)

    val inputPath = args(0)

//    val parserBroadcast = sc.broadcast(new SamplePersonParser)
    val parser = new SamplePersonParser

    val input = sc.textFile(inputPath)

    val parseResults = input.map { line =>
      parser.parse(line)
    }

//    val persons = parseResults.filter {
//      case ParseResult(Some(value), _) => true
//      case _ => false
//    }
//
//    val errors = parseResults.filter {
//      case ParseResult(_, err) if err.nonEmpty => true
//      case _ => false
//    }

    sc.stop()
  }
}
