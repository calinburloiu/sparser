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
trait Parser[I, +O] extends Serializable {

  implicit val conf: ParserConf = ParserConf()

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
    val optionalInput = if (conf.shouldCollectInput) {
      Some(input)
    } else {
      None
    }

    ParseResult.Success(value, optionalInput)
  }
}

/**
 *
 * @param errorCallback a side effect function to be called each time an error occurs
 * @param shouldCollectInput whether to collect the input into each [[ParseResult]]
 * @param shouldCollectErrorMessages whether to collect verbose messages into each [[ParseError]]
 * @param shouldCollectErrorArgs whether to collect tracking args into each [[ParseError]]
 */
case class ParserConf(
    errorCallback: ParseError => Unit = { e: ParseError => () },
    shouldCollectInput: Boolean = true,
    shouldCollectErrorMessages: Boolean = true,
    shouldCollectErrorArgs: Boolean = true) extends Serializable

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
 * @param valueOption optional value resulted from parsing
 * @param errors a list of errors collected during parsing
 * @tparam O type of the value resulted from parsing
 */
sealed abstract class ParseResult[I, +O](
    val valueOption: Option[O],
    val errors: Seq[ParseError],
    val input: Option[I])(implicit conf: ParserConf) extends Serializable {

  import ParseResult._

  def get: O

  def hasValue: Boolean = valueOption.isDefined

  def transform[OO](f: O => TransformResult[OO]): ParseResult[I, OO] =
      valueOption.fold[ParseResult[I, OO]](this.asInstanceOf[ParseResult[I, OO]]) { v =>
    f(v) match {
      // FIXME
      case TransformFailure(error) => Failure(errors, input).reportError(error)
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
      Success(newValue, input)
    } else {
      Warning(newValue, errors, input)
    }

  /**
   * Create a new result, while adding an error to the error list and keeping the old value.
   * @param error to be added
   * @return a new ParserResult
   */
  def reportError(error: ParseError): ParseResult[I, O] = {
    conf.errorCallback(error)
    Failure(errors :+ error.strip, input)
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
    Warning(newValue, errors :+ error.strip, input)
  }
}

object ParseResult {
  case class Success[I, O](
      value: O,
      override val input: Option[I] = None)(implicit conf: ParserConf)
      extends ParseResult(Some(value), Seq(), input)(conf) {

    override def get: O = value
    override def hasValue: Boolean = true
  }

  case class Warning[I, O](
      value: O,
      override val errors: Seq[ParseError],
      override val input: Option[I] = None)(implicit conf: ParserConf)
      extends ParseResult(Some(value), errors, input)(conf) {

    override def get: O = value
    override def hasValue: Boolean = true
  }

  case class Failure[I](
      override val errors: Seq[ParseError],
      override val input: Option[I] = None)(implicit conf: ParserConf)
      extends ParseResult[I, Nothing](None, errors, input)(conf) {

    override def get: Nothing = throw new NoSuchElementException
    override def hasValue: Boolean = false
  }

  def unapply[I, O](result: ParseResult[I, O]): Option[(Option[O], Seq[ParseError], Option[I])] =
    Some((result.valueOption, result.errors, result.input))
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
    val args: Seq[Any]) extends Serializable {

  /**
   * Make a copy of this error without the message and/or args if [[ParserConf]] is set to do so.
   *
   * If [[ParserConf.shouldCollectErrorMessages]] is false a new [[ParseError]] is returned with
   * None as message. If [[ParserConf.shouldCollectErrorArgs]] is false a new [[ParseError]] is
   * returned with and empty list of args.
   * @param conf a configuration object which specifies if message and/or args needs to be stripped
   * @return
   */
  def strip(implicit conf: ParserConf): ParseError = {
    if (conf.shouldCollectErrorMessages && conf.shouldCollectErrorArgs) {
      this
    } else {
      val strippedMessage = if (conf.shouldCollectErrorMessages) message else None
      val strippedArgs = if (conf.shouldCollectErrorArgs) args else Seq()
      new ParseError(name, strippedMessage, strippedArgs)
    }
  }

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
