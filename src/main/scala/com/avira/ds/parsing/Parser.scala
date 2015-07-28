package com.avira.ds.parsing

/**
 * Classes of this trait should be able to transform input text (like a log line) into a
 * structured object value by using `parse` method.
 *
 * The resulted value is wrapped into a [[ParseResult]] which allows the value to be absent,
 * errors to be collected and a callback function (with side effects) to be called each time an
 * error occurs. Check [[ParseResult]] documentation for more information.
 *
 * @tparam O structured object type
 */
trait Parser[I, O] {

  val conf: ParserConf

  /**
   * Function to be called each time an error occurs. Default to do nothing.
   */
  def errorCallback: ParseError => Unit = conf.errorCallback

  /**
   * Transform a string into a structured object wrapped in a [[ParseResult]].
   * @param input string to transform
   * @return structured object
   */
  def parse(input: I): ParseResult[I, O]

  /**
   * Should be called by [[parse()]] method during initialization to create a result object from
   * the input.
   *
   * It makes sure that error callback function (and potentially other things) are passed to the
   * result
   * @param value initial value to be filled in the initial result object
   * @tparam T type of the initial value
   * @return a result containing the input passed the correct error callback
   */
  def createResult[T](value: T, input: I): ParseResult[I, T] = {
    val optionalInput = if (conf.collectInput) {
      Some(input)
    } else {
      None
    }

    ParseSuccessResult(value, optionalInput, conf)
  }
}

/**
 *
 * @param errorCallback a side effect function to be called each time an error occurs
 * @param collectInput whether to collect the input into each [[ParseResult]]
 * @param collectErrorMessages whether to collect verbose messages into each [[ParseError]]
 * @param collectErrorArgs whether to collect tracking args into each [[ParseError]]
 */
case class ParserConf(
    errorCallback: ParseError => Unit = { e: ParseError => () },
    collectInput: Boolean = true,
    collectErrorMessages: Boolean = true,
    collectErrorArgs: Boolean = true)

// TODO Doc missing params
/**
 * Instances of this class are produced by [[Parser]] classes and store an optional value and
 * a list of parsing errors. A callback function (with side effects) can be called each time an
 * error occurs.
 *
 * As the input is being parsed by a [[Parser]], the [[ParseResult]] collects a list of errors as
 * [[ParseError]] objects. If the last error in the list is a fatal one, no value will be present
 * in the [[ParseResult]]. If the value is available errors might indicate warnings or the fact
 * that the parsed value is partial.
 *
 * @param optionalValue optional value resulted from parsing
 * @param errors a list of errors collected during parsing
 * @tparam O type of the value resulted from parsing
 */
sealed abstract class ParseResult[I, +O](
    optionalValue: Option[O],
    val errors: Seq[ParseError],
    input: Option[I],
    conf: ParserConf = ParserConf()) {

  def get: O

  def hasValue: Boolean = optionalValue.isDefined

  def transform[OO](f: O => TransformResult[OO]): ParseResult[I, OO] =
      optionalValue.fold[ParseResult[I, OO]](this.asInstanceOf[ParseResult[I, OO]]) { v =>
    f(v) match {
      // FIXME
      case TransformFailure(error) => ParseErrorResult(errors, input, conf).reportError(error)
      case TransformWarning(newValue, warning) => fillValueAndReportError(newValue, warning)
      case TransformSuccess(newValue) => fillValue(newValue)
    }
  }

  /**
   * Create a new result with the passed value, while keeping errors and the callback.
   * @param newValue new value
   * @tparam OO new type of the value
   * @return a new ParserResult
   */
  def fillValue[OO](newValue: OO): ParseResult[I, OO] =
    if (errors.isEmpty) {
      ParseSuccessResult(newValue, input, conf)
    } else {
      ParseWarningResult(newValue, errors, input, conf)
    }

  /**
   * Create a new result, while adding an error to the error list and keeping the old value.
   * @param error to be added
   * @return a new ParserResult
   */
  def reportError(error: ParseError): ParseResult[I, O] = {
    conf.errorCallback(error)
    ParseErrorResult(errors :+ error, input, conf)
  }

  /**
   * Equivalent of calling `fillValue` and `reportError` in sequence.
   * @param newValue new value
   * @param error to be added
   * @tparam OO new type of the value
   * @return a new ParserResult
   */
  def fillValueAndReportError[OO](newValue: OO, error: ParseError): ParseResult[I, OO] = {
    conf.errorCallback(error)
    ParseWarningResult(newValue, errors :+ error, input, conf)
  }
}

case class ParseSuccessResult[I, O](
    value: O,
    input: Option[I] = None,
    conf: ParserConf = ParserConf()) extends ParseResult(Some(value), Seq(), input, conf) {

  override def get: O = value
  override def hasValue: Boolean = true
}

case class ParseWarningResult[I, O](
    value: O,
    override val errors: Seq[ParseError],
    input: Option[I] = None,
    conf: ParserConf = ParserConf()) extends ParseResult(Some(value), errors, input, conf) {

  override def get: O = value
  override def hasValue: Boolean = true
}

case class ParseErrorResult[I](
    override  val errors: Seq[ParseError],
    input: Option[I] = None,
    conf: ParserConf = ParserConf()) extends ParseResult(None, errors, input, conf) {

  override def get: Nothing = throw new NoSuchElementException
  override def hasValue: Boolean = false
}

object ParseResult {

//  def apply[Nothing](): ParseResult[Nothing] = ParseResult(None, Seq())
//
//  def apply[T](value: T): ParseResult[T] = ParseResult(Some(value), Seq())
//
//  def apply[T](value: T, errorCallback: (ParseError => Unit)): ParseResult[T] =
//    ParseResult(Some(value), Seq(), errorCallback)
//
//  def apply[T](value: T, error: ParseError): ParseResult[T] =
//    ParseResult(Some(value), Seq(error))
//
//  def apply[T](value: T, error: ParseError,
//      errorCallback: (ParseError => Unit)): ParseResult[T] =
//    ParseResult(Some(value), Seq(error), errorCallback)
//
//  def apply[Nothing](errorCallback: (ParseError => Unit)): ParseResult[Nothing] =
//    ParseResult(None, Seq(), errorCallback)

  //  def unapply[T](result: ParseResult[T]): Option[(T, Seq[ParseError])] = result match {
  //    case ParseResult(Some(value), err, _) => Some((value, err))
  //    case _ => None
  //  }

  //  def unapply[T](result: ParseResult[T]): Option[(Option[T], Seq[ParseError])] = result match {
  //    case ParseResult(value, err, _) => Some((value, err))
  //    case _ => None
  //  }
}

sealed trait TransformResult[+T]
case class TransformFailure(error: ParseError) extends TransformResult[Nothing]
case class TransformWarning[T](value: T, warning: ParseError) extends TransformResult[T]
case class TransformSuccess[T](value: T) extends TransformResult[T]

/**
 * Class which abstracts an error returned by a [[Parser]] implementation.
 *
 * You may extend this class for specific errors
 * @param name short name which identifies the error (e.g. "age.invalidNumber")
 * @param message a more detailed description of the error
 *                (e.g. "Invalid age xyz, you should provide a positive integer")
 * @param args sequence of object which might help while debugging the error
 */
class ParseError(
    val name: String,
    val message: Option[String],
    val args: Seq[Any]) {

  def canEqual(other: Any): Boolean = other match {
    case _: ParseError => true
    case _ => false
  }

  override def equals(other: Any): Boolean = other match {
    case that: ParseError =>
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


  override def toString: String = s"ParserError($name, $message, $args)"
}

object ParseError {
  def apply(name: String, message: Option[String], args: Any*): ParseError = new ParseError(
      name, message, args)

  def apply(name: String): ParseError = new ParseError(name, None, Seq())

  def unapply(error: ParseError): Option[(String, Option[String], Seq[Any])] = Some(
    (error.name, error.message, error.args)
  )
}
