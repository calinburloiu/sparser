package com.avira.ds

import com.avira.ds.parsing.{Parser, ParserConf, SamplePersonParser, SamplePerson}

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

object Draft {

  def getFieldValue[O: ClassTag](obj: O, fieldName: String): Any = {
    val mirror = ru.runtimeMirror(getClass.getClassLoader)
    val fieldTermSymbol = ru.typeOf[SamplePerson].declaration(ru.newTermName(fieldName)).asTerm
    val objMirror = mirror.reflect(obj)
    val fieldMirror = objMirror.reflectField(fieldTermSymbol)
    fieldMirror.get
  }

  def f[I, O: ClassTag](parser: Parser[I, O], input: I, fieldName: String): Any = {
    val value = parser.parse(input).valueOption.get
    getFieldValue(value, fieldName)
  }

  def main(args: Array[String]): Unit = {
    val parser = new SamplePersonParser(ParserConf())
    println(f(parser, "Calin\t28", "name"))
  }
}
