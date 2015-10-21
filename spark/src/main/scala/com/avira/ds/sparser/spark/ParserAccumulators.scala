/*
 * Copyright 2015 Avira Operations GmbH
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package com.avira.ds.sparser.spark

import com.avira.ds.sparser.ParseError
import org.apache.spark.{Accumulator, SparkContext}

/** Manages accumulators incremented in real-time when a parser reports an error.
  *
  * One accumulator is mapped to each kind of error reportable by a parser.
  * SParser uses classes extended from `com.avira.ds.sparser.ParseError` each
  * kind of error. Parser which follow the recommended convention has method
  * `parseErrorClasses` in their companion object which retrieves a set of all
  * `ParseError` classes reportable by that parser.
  *
  * You can construct an instance of this class by using `apply` from the
  * companion object which accepts a `SparkContext` and the set of `ParseError`
  * classes mentioned.
  *
  * Use method `createAccumulatorsParserCallback` of this class to create an
  * error callback function required by parsers to integrate with Spark
  * accumulators: Here is how you can do it:
  *
  * {{{
  * val parserAccumulators = ParserAccumulators(sc, SamplePersonParser.parseErrorClasses)
  * val parserConf: ParserConf = ParserConf(
  *   errorCallback = parserAccumulators.createAccumulatorsParserCallback
  * )
  * implicit val parser: Parser[String, SamplePerson] = new SamplePersonParser(parserConf)
  * }}}
  */
case class ParserAccumulators(
    accumulators: Map[String, Accumulator[Long]],
    parseErrorClasses: Set[Class[_ <: ParseError]]) extends Serializable {

  /** Creates an error callback function required by parsers to integrate with
    * Spark accumulators.
    */
  def createAccumulatorsParserCallback: ParseError => Unit = { err: ParseError =>
    accumulators.get(err.getClass.getCanonicalName).foreach { acc =>
      acc += 1L
    }
  }
}

/** Companion object of [[ParserAccumulators]] used as a factory for it */
object ParserAccumulators {

  /** Factory method for creating [[ParserAccumulators]] classes
    *
    * @param sc `SparkContext` used in your application
    * @param parseErrorClasses set of all `ParseError` classes reportable by your parser
    */
  def apply(
      sc: SparkContext,
      parseErrorClasses: Set[Class[_ <: ParseError]]): ParserAccumulators = {
    val accumulators: Map[String, Accumulator[Long]] = parseErrorClasses.map { clazz =>
      val name = clazz.getCanonicalName
      (name, sc.accumulator(0L, name))
    }.toMap

    new ParserAccumulators(accumulators, parseErrorClasses)
  }
}
