package com.avira.ds.sparser.spark

import com.avira.ds.sparser.{ParseResult, Parser}
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag

object SparserSpark {

  implicit class ParserGeneratorRDDFunctions[I](rdd: RDD[I]) {

    def parse[O: ClassTag](
        parserGenerator: () => Parser[I, O]): RDD[O] = {
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

    def parse[O](implicit ev: ClassTag[O], parserGenerator: () => Parser[I, O]): RDD[O] = {
      parse(parserGenerator)(ev)
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

  implicit class ParserRDDFunctions[I](rdd: RDD[I]) {

    def parse[O: ClassTag](
        parser: Parser[I, O]): RDD[O] = {
      implicit val parserGenerator: () => Parser[I, O] = { () => parser }
      new ParserGeneratorRDDFunctions(rdd).parse
    }

    def parse[O](implicit ev: ClassTag[O], parser: Parser[I, O]): RDD[O] = {
      parse(parser)(ev)
    }

    def parseWithErrors[O](
        implicit parser: Parser[I, O]): RDD[ParseResult[I, O]] = {
      implicit val parserGenerator: () => Parser[I, O] = { () => parser }
      new ParserGeneratorRDDFunctions(rdd).parseWithErrors
    }
  }

  implicit class ParserBroadcastRDDFunctions[I](rdd: RDD[I]) {

    def parse[O: ClassTag](
        parserBroadcast: Broadcast[Parser[I, O]]): RDD[O] = {
      implicit val parserGenerator: () => Parser[I, O] = { () => parserBroadcast.value }
      new ParserGeneratorRDDFunctions(rdd).parse
    }

    def parse[O: ClassTag](
        implicit ev: ClassTag[O], parserBroadcast: Broadcast[Parser[I, O]]): RDD[O] = {
      parse(parserBroadcast)(ev)
    }

    def parseWithErrors[O](
        implicit parserBroadcast: Broadcast[Parser[I, O]]): RDD[ParseResult[I, O]] = {
      implicit val parserGenerator: () => Parser[I, O] = { () => parserBroadcast.value }
      new ParserGeneratorRDDFunctions(rdd).parseWithErrors
    }
  }
}
