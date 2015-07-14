package com.avira.ds.parsing

/**
 * Classes of this trait should be able to transform input text (like a log line) into a
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
   * @param input string to transform
   * @return structured object
   */
  def parse(input: String): ParserResult[A]

  /**
   * Should be called by [[parse()]] method during initialization to create a result object from
   * the input.
   *
   * It makes sure that error callback function (and potentially other things) are passed to the
   * result
   * @param input initial value to be filled in the initial result object
   * @tparam B type of the initial value
   * @return a result containing the input passed the correct error callback
   */
  def createResult[B](input: B): ParserResult[B] = ParserResult(input, errorCallback)
}

sealed trait PipeResult[+R]
//case object PipeSkip extends PipeResult[Nothing]
case class PipeFailure(error: ParserError) extends PipeResult[Nothing]
case class PipeWarning[R](value: R, warning: ParserError) extends PipeResult[R]
case class PipeSuccess[R](value: R) extends PipeResult[R]

/**
 * Instances of this class are produced by [[Parser]] classes and store an optional value and
 * a list of parsing errors. A callback function (with side effects) can be called each time an
 * error occurs.
 *
 * As the input is being parsed by a [[Parser]], the [[ParserResult]] collects a list of errors as
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

  def pipe2[B](f: A => PipeResult[B]): ParserResult[B] = value.fold[ParserResult[B]](
    copy(value = None)
  ) { v =>
    f(v) match {
      case PipeFailure(error) => copy(value = None).reportError(Some(error))
      case PipeWarning(newValue, warning) => fillValue(Some(newValue)).reportError(Some(warning))
      case PipeSuccess(newValue) => fillValue(Some(newValue))
    }
  }

  def pipe[B](f: (Option[A] => (Option[B], Option[ParserError]))): ParserResult[B] = {
    val (newValue, error) = f(value)
    fillValue(newValue).reportError(error)
  }

  /**
   * Create a new result with the passed value, while keeping errors and the callback.
   * @param newValue new value
   * @tparam B new type of the value
   * @return a new ParserResult
   */
  def fillValue[B](newValue: Option[B]): ParserResult[B] = this.copy(value = newValue)

  /**
   * Add an error to the error list.
   * @param error to be added
   * @return a new ParserResult
   */
  def reportError(error: Option[ParserError]): ParserResult[A] = error.fold(this) { e =>
    errorCallback(e)
    this.copy(errors = errors :+ e)
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
