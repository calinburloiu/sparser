package com.avira.ds.sparser.spark

import com.avira.ds.sparser.{ParseResult, Parser}
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag

/** Implicit definitions which allow parsing data in Spark by directly
  * calling `parse` or `parseWithErrors` method on an `RDD`.
  *
  * Spark is a distributed system and your parser needs to run on workers.
  * There are multiple ways to deploy parser instances to workers. For each one
  * you need a different implicit class to import in your scope to make SParser
  * work with Spark. Don't import all of them! Here the ''parser deploy modes''
  * along with the import statement required:
  *
  *  1. '''Simple''': the parser instance is created in the driver and the
  *     framework will serialize it and send it to the workers.
  *    - `import [[com.avira.ds.sparser.spark.SparserSpark.ParserSimpleRDDFunctions]]`
  *  1. '''Parser Generator''': the parser is created on workers by a lambda
  *     function named ''parser generator'', `() => Parser[I, O]`.
  *    - `import [[com.avira.ds.sparser.spark.SparserSpark.ParserGeneratorRDDFunctions]]`
  *  1. '''Broadcast''': the parser instance is sent via a broadcast variable.
  *    - `import [[com.avira.ds.sparser.spark.SparserSpark.ParserBroadcastRDDFunctions]]`
  *
  * All of the above parser deploy modes allow parsing by calling one of the
  * methods directly on RDDs:
  *
  *  - Method `parseWithErrors` returns a new RDD with
  *  `com.avira.ds.sparser.ParseResult` elements. The user is than responsible
  *  to extract the value or errors from this objects.
  *  - Method `parse` returns a new RDD which directly contains the output
  *  value, so extracting errors is no longer possible.
  *
  * Both methods accept an implicit argument which can be a parser instance for
  * ''Simple'' deploy mode, a parser generator function for ''Parser
  * Generator'' deploy mode or a broadcast variable with a parser instance for
  * ''Broadcast'' deploy mode.
  */
object SparserSpark {

  /** Import this class in your scope for '''Parser Simple Deploy Mode'''.
    *
    * In this deploy mode the parser instance is created in the driver and the
    * framework will serialize it and send it to the workers.
    *
    * This method is very inefficient for large space parser instances because
    * the same data is copied repeatedly to all workers. So it is only suited
    * for simplicity or for very simple parsers.
    *
    * @tparam I Parser input type
    */
  implicit class ParserSimpleRDDFunctions[I](rdd: RDD[I]) {

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

  /** Import this class in your scope for '''Parser Generator Deploy Mode'''.
    *
    * In this deploy mode the parser is created on workers by a lambda function
    * named ''parser generator'', `() => Parser[I, O]`.
    *
    * A small amount of data is transfered from driver to worker (the parser
    * generator is very small) and instances are created in parallel, but if
    * parser creation requires an expensive request to an external service or
    * database it might overload it (e.g. requesting currency exchange data for
    * the past year to a web service from 1000 workers).
    *
    * @tparam I Parser input type
    */
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

  /** Import this class in your scope for '''Parser Broadcast Deploy Mode'''.
    *
    * In this deploy mode the parser instance is sent via a broadcast variable.
    *
    * This mode is more efficient than the Simple mode since the data is sent
    * in a BitTorrent manner. You may decide not to use it if you have
    * serialization problems, such as getting `NotSerializableException` for
    * Java serialization or complicated class registration for Kryo
    * serialization.
    *
    * @tparam I Parser input type
    */
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

