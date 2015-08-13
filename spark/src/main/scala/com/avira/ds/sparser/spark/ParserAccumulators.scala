package com.avira.ds.sparser.spark

import com.avira.ds.sparser.ParseError
import org.apache.spark.{Accumulator, SparkContext}

case class ParserAccumulators(
    accumulators: Map[String, Accumulator[Long]],
    parseErrorClasses: Set[Class[_ <: ParseError]]) extends Serializable {

  def createAccumulatorsParserCallback: ParseError => Unit = { err: ParseError =>
    accumulators.get(err.getClass.getCanonicalName).foreach { acc =>
      acc += 1L
    }
  }
}

object ParserAccumulators {

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
