package com.avira.ds.parsing


trait Parser[T] {
  def parse(line: String): Either[ParserError, T]
}

case class ParserError(messages: Seq[String], line: Option[String]) {

  def :+(message: String): ParserError = ParserError(messages :+ message, line)
  def append(message: String): ParserError = this :+ message

  def +:(message: String): ParserError = ParserError(message +: messages, line)
  def prepend(message: String): ParserError = message +: this
}

object ParserError {
  def apply(): ParserError = ParserError(Seq(), None)
  def apply(message: String): ParserError = ParserError(Seq(message))
  def apply(messages: Seq[String]): ParserError = ParserError(messages, None)
  def apply(message: String, line: String): ParserError = ParserError(Seq(message), Some(line))
}