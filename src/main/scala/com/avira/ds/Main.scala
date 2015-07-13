package com.avira.ds

import com.typesafe.scalalogging.slf4j.StrictLogging
import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.SparkContext._

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
    val sc = new SparkContext(conf)

    // TODO Add your code here.
    logger.info("Hello, world!")

    sc.stop()
  }
}
