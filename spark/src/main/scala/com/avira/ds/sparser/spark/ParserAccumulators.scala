package com.avira.ds.sparser.spark

import com.avira.ds.sparser.ParseError
import org.apache.spark.{Accumulator, SparkContext}

class ParserAccumulators(
    sc: SparkContext,
    parseErrorClasses: Set[Class[_ <: ParseError]]) {

  lazy val accumulators: Map[String, Accumulator[Long]] = parseErrorClasses.map { clazz =>
    val name = clazz.getCanonicalName
    (name, sc.accumulator(0L, name))
  }.toMap

  def createAccumulatorsParserCallback: ParseError => Unit = { err: ParseError =>
    accumulators.get(err.getClass.getCanonicalName).foreach { acc =>
      acc += 1L
    }
  }
}
