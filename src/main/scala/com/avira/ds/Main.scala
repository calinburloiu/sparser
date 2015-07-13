package com.avira.ds

import com.typesafe.scalalogging.slf4j.StrictLogging
import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.SparkContext._

import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global

import play.api.libs.json._

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
//    val conf = new SparkConf()
//    val sc = new SparkContext(conf)
//    sc.stop()

    // scalastyle:off
    val jsonStr = """{"m": "Avira.ServiceHost.exe", "c": "3XX", "l": "Error", "d": "da9191dbf2114dbc9e3bd2df6b193f36c9272ab2", "ct": "WIN", "p": { "i": "Microsoft Windows NT 5.1.2600 Service Pack 3", "m": "2015-07-11 23:00:31.7187 [Error] [Avira.OE.WinCore.ManifestSignatureValidator] Not able to validate the manifest signature. System.Exception: Can not read the 'data' property from the manifestdata.\r\n   em Avira.OE.WinCore.ManifestSignatureValidator.CheckManifestDataValid(JsonObject manifestData, String propertyName)\r\n   em Avira.OE.WinCore.ManifestSignatureValidator.GetDataFromManifest(JsonObject manifestData, String propertyName)\r\n   em Avira.OE.WinCore.ManifestSignatureValidator.IsSignatureValid(JsonObject jsonManifestData)" }}"""
    // scalastyle:on

    val json = Json.parse(jsonStr)

    println((json \ "p" \ "m").validate[String])
  }
}
