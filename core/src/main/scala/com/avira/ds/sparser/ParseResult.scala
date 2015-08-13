package com.avira.ds.sparser

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
    Failure(errors :+ error, input)
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
    Warning(newValue, errors :+ error, input)
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
