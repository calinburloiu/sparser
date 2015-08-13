package com.avira.ds.sparser

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
 */
case class ParserConf(
    errorCallback: ParseError => Unit = { e: ParseError => () },
    shouldCollectInput: Boolean = true) extends Serializable
