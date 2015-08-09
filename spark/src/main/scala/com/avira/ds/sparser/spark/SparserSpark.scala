package com.avira.ds.sparser.spark

import com.avira.ds.sparser.{ParseResult, Parser, ParseError}
import org.apache.spark.Accumulator
import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag

object SparserSpark {

  implicit class SparserRDDFunctions[I](rdd: RDD[I]) {

    def parse[O: ClassTag](
        implicit parserGenerator: () => Parser[I, O]): RDD[O] = {
      rdd.mapPartitions { inputs =>
        val parser = parserGenerator()

        inputs.flatMap { input =>
          parser.parse(input) match {
            case ParseResult.Success(value, _) => Some(value)
            case ParseResult.Warning(value, _, _) => Some(value)
            case ParseResult.Failure(_, _) => None
          }
        }
      }
    }

    def parseWithErrors[O](
        implicit parserGenerator: () => Parser[I, O]): RDD[ParseResult[I, O]] = {
      rdd.mapPartitions { inputs =>
        val parser = parserGenerator()

        inputs.map { input =>
          parser.parse(input)
        }
      }
    }
  }
}
