package com.avira.ds.sparser

/** Instances of this class are produced by [[Parser]] classes and store an
  * optional value and a list of parsing errors. A callback function (with side
  * effects) can be called each time an error occurs.
  *
  * As the input is being parsed by a [[Parser]], the [[ParseResult]] collects a
  * list of errors as [[ParseError]] objects. If the last error in the list is a
  * fatal one, no value will be present in the [[ParseResult]]. If the value is
  * available, errors might indicate warnings or the fact that the parsed value
  * is partial.
  *
  * Parser users only have to deal with the value and the errors inside the
  * parser. It might be helpful for them to do pattern matching on
  * [[ParseResult]] case classes:
  *
  * {{{
  * result match {
  *   case ParseResult.Success(value, input) => ???
  *   case ParseResult.Warning(value, errors, input) => ???
  *   case ParseResult.Failure(errors, input) => ???
  * }
  * }}}
  *
  * Parser developers, who are responsible to create new parsers, will
  * additionally have to learn to operate with the `transform` method. Check
  * [[com.avira.ds.sparser.samples.SamplePersonParser]] for an example.
  *
  * @param valueOption Optional value resulted from parsing
  * @param errors A list of errors collected during parsing
  * @param input If configured via [[ParserConf]] the result can track the input
  * that lead to output result, feature that might be useful while debugging
  * errors
  * @param conf Parser configuration object which decides whether input should
  * be collected or not
  * @tparam I Type of the input accepted by the parser
  * @tparam O Type of the value resulted from parsing
  * @see [[ParseError]] and [[TransformResult]]
  */
sealed abstract class ParseResult[I, +O](
    val valueOption: Option[O],
    val errors: Seq[ParseError],
    val input: Option[I])(implicit conf: ParserConf) extends Serializable {

  import ParseResult._

  /** Returns result's value if available.
    *
    * @throws NoSuchElementException if the result does not contain any value
    * @return value if available
    */
  def get: O

  /** Whether the result has a value inside */
  lazy val hasValue: Boolean = valueOption.isDefined

  /** Whether the result has at least one error */
  lazy val hasErrors: Boolean = errors.nonEmpty

  /** Returns true if the result contains a value and there are no errors */
  def isSuccess: Boolean

  /** Returns true if the result contains a value, but there were also some errors */
  def isWarning: Boolean

  /** Returns true if the result contains no value because of at least one error */
  def isFailure: Boolean

  /** Main method used by parser developers to incrementally transform input
    * data into ''value''.
    *
    * This is basically a `flatMap` function which accepts a function
    * responsible to transform the wrapped value. But the accepted lambda
    * function does not directly return the new wrapped value as it happens
    * with a traditional `flatMap`. It returns instead a [[TransformResult]]
    * object which allows wrapping an error along with the optional value.
    * There are three sealed cases: [[TransformSuccess]], [[TransformWarning]]
    * and [[TransformFailure]].  Check [[TransformResult]] documentation to
    * learn more.
    */
  def transform[OO](f: O => TransformResult[OO]): ParseResult[I, OO] =
      valueOption.fold[ParseResult[I, OO]](this.asInstanceOf[ParseResult[I, OO]]) { v =>
    f(v) match {
      case TransformFailure(error) => Failure(errors, input).reportError(error)
      case TransformWarning(newValue, warning) => fillValueAndReportError(newValue, warning)
      case TransformSuccess(newValue) => fillValue(newValue)
    }
  }

  /** Create a new result with the passed value, while keeping errors and the callback.
    *
    * @param newValue New value
    * @tparam OO New type of the value
    * @return A new ParserResult
    */
  private[sparser] def fillValue[OO](newValue: OO): ParseResult[I, OO] =
    if (errors.isEmpty) {
      Success(newValue, input)
    } else {
      Warning(newValue, errors, input)
    }

  /** Create a new result, while adding an error to the error list and keeping the old value.
    * @param error Error to be added to the list of tracked errors
    * @return A new ParserResult
    */
  private[sparser] def reportError(error: ParseError): ParseResult[I, O] = {
    conf.errorCallback(error)
    Failure(errors :+ error, input)
  }

  /** Equivalent of calling `fillValue` and `reportError` in sequence.
    * @param newValue New value
    * @param error Error to be added to the list of tracked errors
    * @tparam OO New type of the value
    * @return A new ParserResult
    */
  private[sparser] def fillValueAndReportError[OO](
      newValue: OO, error: ParseError): ParseResult[I, OO] = {
    conf.errorCallback(error)
    Warning(newValue, errors :+ error, input)
  }
}

/** Companion object of [[ParseResult]] which contains contains all concrete
  * child case classes and other utility methods.
  */
object ParseResult {

  /** [[Parser]] result which contains a value and no errors */
  case class Success[I, O](
      value: O,
      override val input: Option[I] = None)(implicit conf: ParserConf)
    extends ParseResult(Some(value), Seq(), input)(conf) {

    override val get: O = value
    override lazy val hasValue: Boolean = true
    override lazy val hasErrors: Boolean = false
    override val isSuccess: Boolean = true
    override val isWarning: Boolean = false
    override val isFailure: Boolean = false
  }

  /** [[Parser]] result which contains a (potentially incomplete) value and at least one error */
  case class Warning[I, O](
      value: O,
      override val errors: Seq[ParseError],
      override val input: Option[I] = None)(implicit conf: ParserConf)
    extends ParseResult(Some(value), errors, input)(conf) {

    override val get: O = value
    override lazy val hasValue: Boolean = true
    override val isSuccess: Boolean = false
    override val isWarning: Boolean = true
    override val isFailure: Boolean = false
  }

  /** [[Parser]] result which doesn't contain a value and as a consequence
    * it has at least one error
    */
  case class Failure[I](
      override val errors: Seq[ParseError],
      override val input: Option[I] = None)(implicit conf: ParserConf)
    extends ParseResult[I, Nothing](None, errors, input)(conf) {

    override def get: Nothing = throw new NoSuchElementException
    override lazy val hasValue: Boolean = false
    override lazy val isSuccess: Boolean = false
    override lazy val isWarning: Boolean = false
    override lazy val isFailure: Boolean = true
  }

  /** Used for pattern matching of generic results which have an optional
    * value, a list of errors and the input for which parsing was attempted.
    */
  def unapply[I, O](result: ParseResult[I, O]): Option[(Option[O], Seq[ParseError], Option[I])] =
    Some((result.valueOption, result.errors, result.input))
}
