package com.avira.ds.sparser

/**
 * Classes of this trait should be able to transform an input of type `I` (e.g.
 * a log line) into a ''value'' (e.g. a structured object) of type `O` by using
 * `parse` method.
 *
 * The resulted value is wrapped into a [[com.avira.ds.sparser.ParseResult]]
 * which allows the value to be absent, errors to be collected and a callback
 * function (with side effects) to be called each time an error occurs. Check
 * [[com.avira.ds.sparser.ParseResult]] documentation for more information.
 *
 * Example usage for a sample parser implementation available in the library:
 *
 * {{{
 * scala> :paste
 * // Entering paste mode (ctrl-D to finish)
 *
 * import com.avira.ds.sparser.samples.SamplePersonParser
 * import com.avira.ds.sparser.ParserConf
 *
 * val parser = new SamplePersonParser(ParserConf())
 *
 * // Exiting paste mode, now interpreting.
 *
 * import com.avira.ds.sparser.samples.SamplePersonParser
 * import com.avira.ds.sparser.ParserConf
 * parser: com.avira.ds.sparser.samples.SamplePersonParser = [...]
 *
 * scala> val res = parser.parse("John Doe\t25")
 * res: com.avira.ds.sparser.ParseResult[String,com.avira.ds.sparser.samples.SamplePerson] = [...]
 *
 * scala> val person = res.valueOption.get
 * person: com.avira.ds.sparser.samples.SamplePerson = SamplePerson(John Doe,25)
 *
 * scala> person.name
 * res4: String = John Doe
 *
 * scala> person.age
 * res5: Int = 25
 * }}}
 *
 * @tparam I input type
 * @tparam O output value type
 */
trait Parser[I, +O] extends Serializable {

  implicit val conf: ParserConf = ParserConf()

  /** Function to be called each time an error occurs. Default to do nothing. */
  def errorCallback: ParseError => Unit = conf.errorCallback

  /** Transforms a string into a structured object wrapped in a
    * [[com.avira.ds.sparser.ParseResult]].
    *
    * @param input data to parse
    * @return value extracted from the input
    */
  def parse(input: I): ParseResult[I, O] = parse(createResult(input))

  protected def parse(initResult: ParseResult[I, I]): ParseResult[I, O]

  /** Factory method for creating [[com.avira.ds.sparser.ParseResult]]
    * instances.
    * 
    * Should be used by `parse` method during initialization to create the
    * initial result object from the input. It makes sure that configuration
    * parameters from [[com.avira.ds.sparser.ParserConf]] (like the error
    * callback function) are passed to the result.
    *
    * @param input data that is going to be parsed
    * @return a result containing the value and input passed as well as the
    * configuration
    */
  protected def createResult(input: I): ParseResult[I, I] = {
    val optionalInput = if (conf.shouldCollectInput) {
      Some(input)
    } else {
      None
    }

    ParseResult.Success(input, optionalInput)
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
