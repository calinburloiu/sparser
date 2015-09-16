SParser
=======

Scala/Spark Parser library allows abstracting input parsing logic into
specialized classes usable both in standalone Scala projects or with Apache
Spark.

Parsing with Scala
------------------

Create your own parsers for your data by extending
[`Parser`](/core/src/main/scala/com/avira/ds/sparser/Parser) trait, which will
allow you to transform the input into a desired value, wrapped into a
[`ParseResult`](/core/src/main/scala/com/avira/ds/sparser/ParseResult) object,
which collects errors as
[`ParseError`](/core/src/main/scala/com/avira/ds/sparser/ParseError) child
class instances.

The parsing process can be divided into one or more monadic operations on
[`ParseResult`](/core/src/main/scala/com/avira/ds/sparser/ParseResult) objects
by using `transform` method which accepts a `flatMap`-like lambda function. The
return type of this function is an instance of
[`TransformResult`](/core/src/main/scala/com/avira/ds/sparser/TransformResult)
which has cases for success, warning and failure results and can encapsulate an
error.

While using SParser you might be a _parser developer_, which creates new parser
for new data types, or a _parser user_, who uses existing parsers in order to
make sense of data.

_Parser users_ generally call `parse` method and need to know how to get the
value and the errors out of
[`ParseResult`](/core/src/main/scala/com/avira/ds/sparser/ParseResult) objects.
Here is an example usage for a sample parser implementation available in the
library:

```scala
scala> :paste
// Entering paste mode (ctrl-D to finish)

import com.avira.ds.sparser.samples.SamplePersonParser
import com.avira.ds.sparser.ParserConf

val parser = new SamplePersonParser(ParserConf())

// Exiting paste mode, now interpreting.

import com.avira.ds.sparser.samples.SamplePersonParser
import com.avira.ds.sparser.ParserConf
parser: com.avira.ds.sparser.samples.SamplePersonParser = [...]

scala> val res = parser.parse("John Doe\t25")
res: com.avira.ds.sparser.ParseResult[String,com.avira.ds.sparser.samples.SamplePerson] = [...]

scala> val person = res.valueOption.get
person: com.avira.ds.sparser.samples.SamplePerson = SamplePerson(John Doe,25)

scala> person.name
res4: String = John Doe

scala> person.age
res5: Int = 25
```

_Parser developers_ need to implement protected abstract method `parse` where
they operate on
[`ParseResult`](/core/src/main/scala/com/avira/ds/sparser/ParseResult) monadic
objects by applying a series of functional transformations. Errors are
identified by classes or objects which implement
[`ParseError`](/core/src/main/scala/com/avira/ds/sparser/ParseError) trait.
[Check some example
parsers](/core/src/main/scala/com/avira/ds/sparser/samples/).

For more information about using SParser check the ScalaDocs of the modules.

Parsing with Spark
------------------

SParser contains tools which help Spark users more easily work with parsers.

Two kind of tools are currently provided:

* Implicit definitions which allow parsing data in Spark by directly calling
`parse` or `parseWithErrors` method on an `RDD`.
* Real-time incrementation of Spark accumulators for errors reported while
parsing by using parser's error callback function. Check
[[com.avira.ds.sparser.spark.ParserAccumulators]].

Importing an implicit definition allows parsing by calling one of the methods
directly on RDDs:

- Method `parseWithErrors` returns a new RDD with
`com.avira.ds.sparser.ParseResult` elements. The user is than responsible to
extract the value or errors from this objects.
- Method `parse` returns a new RDD which directly contains the output value, so
extracting errors is no longer possible.

Basically, you can use SParser with Spark as simple as in this example:

```scala
import com.avira.ds.sparser._
import com.avira.ds.sparser.samples.{SamplePerson, SamplePersonParser}

val input = sc.textFile("/path/to/input")

implicit val parser: Parser[String, SamplePerson] =
    new SamplePersonParser(ParserConf())
val persons = input.parse
```

For a full sample application check [spark-sample/](/spark-sample) directory.
For more information about the integration with Spark check the ScalaDoc of the
_spark_ module.
