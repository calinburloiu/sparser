package com.avira.ds.sparser.spark

import com.avira.ds.sparser.{ParseResult, Parser, ParseError}
import org.apache.spark.Accumulator
import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag

object SparserSpark {

  implicit class SparserRDDFunctions[I](rdd: RDD[I]) {

    def parse[O: ClassTag](implicit parser: Parser[I, O]): RDD[O] = {
      rdd.flatMap { input =>
        parser.parse(input) match {
          case ParseResult.Success(value, _) => Some(value)
          case ParseResult.Warning(value, _, _) => Some(value)
          case ParseResult.Failure(_, _) => None
        }
      }
    }

    def parseWithErrors[O](implicit parser: Parser[I, O]): RDD[ParseResult[I, O]] = {
      rdd.map { input =>
        parser.parse(input)
      }
    }
  }

  def createAccumulatorsParserCallback(
      accumulators: Map[String, Accumulator[Long]]): ParseError => Unit = { err: ParseError =>
    accumulators.get(err.name).foreach { acc =>
      acc += 1L
    }
  }

  def createAccumulatorsParserCallback(
      namedAccumulators: Seq[Accumulator[Long]]): ParseError => Unit = {
    val accumulatorsMap = namedAccumulators.map { acc =>
      if (acc.name.isEmpty) {
        throw new IllegalArgumentException("All accumulators need to be named")
      } else {
        acc.name.get -> acc
      }
    }.toMap

    createAccumulatorsParserCallback(accumulatorsMap)
  }
}
