package com.avira.ds.parsing

/**
 * Classes of this trait should be able to transform a line of text (like a log line) into a
 * structured object value by using `parse` method.
 *
 * The resulted value is wrapped into a [[ParserResult]] which allows the value to be absent,
 * errors to be collected and a callback function (with side effects) to be called each time an
 * error occurs. Check [[ParserResult]] documentation for more information.
 *
 * @tparam A structured object type
 */
trait Parser[A] {
  /**
   * Function to be called each time an error occurs. Default to do nothing.
   */
  val errorCallback: (ParserError => Unit) = { e: ParserError => () }

  /**
   * Transform a string into a structured object wrapped in a [[ParserResult]].
   * @param line string to transform
   * @return structured object
   */
  def parse(line: String): ParserResult[A]

  def emptyResult: ParserResult[A] = ParserResult[A](errorCallback)
}

/**
 * Instances of this class are produced by [[Parser]] classes and store an optional value and
 * a list of parsing errors. A callback function (with side effects) can be called each time an
 * error occurs.
 *
 * As the line is being parsed by a [[Parser]], the [[ParserResult]] collects a list of errors as
 * [[ParserError]] objects. If the last error in the list is a fatal one, no value will be present
 * in the [[ParserResult]]. If the value is available errors might indicate warnings or the fact
 * that the parsed value is partial.
 *
 * @param value optional value resulted from parsing
 * @param errors a list of errors collected during parsing
 * @param errorCallback a side effect function to be called each time an error occurs
 * @tparam A type of the value resulted from parsing
 */
case class ParserResult[+A](
    value: Option[A],
    errors: Seq[ParserError],
    errorCallback: (ParserError => Unit) = { e: ParserError => () }) {

  /**
   * Create a new result with the passed value, while keeping errors and the callback.
   * @param value new value
   * @tparam B new type of the value
   * @return a new ParserResult
   */
  def fillValue[B >: A](value: B): ParserResult[B] = this.copy(value = Some(value))

  /**
   * Add an error to the error list.
   * @param error to be added
   * @return a new ParserResult
   */
  def reportError(error: ParserError): ParserResult[A] = {
    errorCallback(error)
    this.copy(errors = errors :+ error)
  }
}

object ParserResult {
  def apply[T](value: T): ParserResult[T] = ParserResult(Some(value), Seq())

  def apply[T](value: T, errorCallback: (ParserError => Unit)): ParserResult[T] =
    ParserResult(Some(value), Seq(), errorCallback)

  def apply[T](value: T, error: ParserError): ParserResult[T] =
    ParserResult(Some(value), Seq(error))

  def apply[T](value: T, error: ParserError,
      errorCallback: (ParserError => Unit)): ParserResult[T] =
    ParserResult(Some(value), Seq(error), errorCallback)

  def apply[Nothing](errorCallback: (ParserError => Unit)): ParserResult[Nothing] =
    ParserResult(None, Seq(), errorCallback)
}

/**
 * Class which abstracts an error returned by a [[Parser]] implementation.
 *
 * You may extend this class for specific errors
 * @param name short name which identifies the error (e.g. "age.invalidNumber")
 * @param message a more detailed description of the error
 *                (e.g. "Invalid age xyz, you should provide a positive integer")
 * @param args sequence of object which might help while debugging the error
 */
class ParserError(
    val name: String,
    val message: Option[String],
    val args: Seq[Any]) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[ParserError]

  override def equals(other: Any): Boolean = other match {
    case that: ParserError =>
      (that canEqual this) &&
        name == that.name &&
        message == that.message &&
        args == that.args
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name, message, args)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }


  override def toString = s"ParserError($name, $message, $args)"
}

object ParserError {
  def apply(name: String, message: Option[String], args: Any*): ParserError = new ParserError(
      name, message, args)

  def apply(_name: String): ParserError = new ParserError(_name, None, Seq())

  def unapply(error: ParserError): Option[(String, Option[String], Seq[Any])] = Some(
    (error.name, error.message, error.args)
  )
}
