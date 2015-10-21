/*
 * Copyright 2015 Avira Operations GmbH
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package com.avira.ds.sparser

/**
 * Classes of this trait should be able to transform an input of type `I` (e.g.
 * a log line) into a ''value'' (e.g. a structured object) of type `O` by using
 * `parse` method.
 *
 * The resulted value is wrapped into a [[ParseResult]] which allows the value
 * to be absent, errors to be collected and a callback function (with side
 * effects) to be called each time an error occurs. Check [[ParseResult]]
 * documentation for more information.
 *
 * All [[Parser]] instances contains general configuration object
 * [[ParserConf]].
 *
 * [[Parser]] users generally call `parse` method and need to know how to get
 * the value and the errors out of [[ParseResult]] objects. Here is an example
 * usage for a sample parser implementation available in the library:
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
 * [[Parser]] developers need to implement protected abstract method `parse`
 * where they operate on [[ParseResult]] monadic objects by applying a series
 * of functional transformations. Errors are identified by classes or objects
 * which implement [[ParseError]] trait. Conventionally, [[Parser]] developers
 * should create a companion object for their class with method
 * `parseErrorClasses: Set[Class[_ <: ParseError]]` which returns the set of
 * all [[ParseError]] classes returnable by the parser. Macro method
 * `getSealedClassChildren` from `com.avira.ds.MacroUtils` object allows
 * developers to retrieve all error classes in one line of code provided that
 * all errors of a parser are extending the same sealed abstract class or
 * trait.
 *
 * @tparam I Input type
 * @tparam O Output value type
 * @see [[ParseResult]] and [[ParseError]]
 */
trait Parser[I, +O] extends Serializable {

  implicit val conf: ParserConf = ParserConf()

  /** Function to be called each time an error occurs. Default to do nothing. */
  def errorCallback: ParseError => Unit = conf.errorCallback

  /** Transforms a string into a structured object wrapped in a
    * [[ParseResult]].
    *
    * @param input Data to parse
    * @return Value Extracted from the input
    */
  def parse(input: I): ParseResult[I, O] = parse(createResult(input))

  protected def parse(initResult: ParseResult[I, I]): ParseResult[I, O]

  /** Factory method for creating [[ParseResult]]
    * instances.
    *
    * Should be used by `parse` method during initialization to create the
    * initial result object from the input. It makes sure that configuration
    * parameters from [[ParserConf]] (like the error
    * callback function) are passed to the result.
    *
    * @param input Data that is going to be parsed
    * @return A result containing the value and input passed as well as the
    * configuration
    */
  // FIXME
  // @return A result ... as well as the configuration
  // I think it does not contain configuration
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
 * General configuration class for [[Parser]]s where you can configure error
 * callback function and what should be collected in [[ParseResult]] object.
 *
 * @param errorCallback A side effect function to be called each time an error occurs
 * @param shouldCollectInput Whether to collect the input into each [[ParseResult]]
 */
class ParserConf(
    val errorCallback: ParseError => Unit = { e: ParseError => () },
    val shouldCollectInput: Boolean = true)
  extends Serializable

object ParserConf {
  def apply(errorCallback: ParseError => Unit = { e: ParseError => () },
      shouldCollectInput: Boolean = true): ParserConf =
    new ParserConf(errorCallback, shouldCollectInput)

  def unapply(parserConf: ParserConf): Option[(ParseError => Unit, Boolean)] =
    Some((parserConf.errorCallback, parserConf.shouldCollectInput))
}
