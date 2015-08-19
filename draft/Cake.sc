trait OutputMapperComponent[O] {

  val outputMapper: OutputMapper

  trait OutputMapper {
    def map(output: O): O
  }
}

trait ParserComponent[I, O] {
  this: OutputMapperComponent[O] =>

  val parser: Parser

  trait Parser {
    def parse(input: I): O

    def parseAndMap(input: I) = outputMapper.map(parse(input))
  }
}

trait StarOutputMapperComponent extends OutputMapperComponent[String] {
  override val outputMapper: OutputMapper = new StarOutputMapper

  class StarOutputMapper extends OutputMapper {
    override def map(output: String): String = s"*$output"
  }
}

class IntParserComponent extends ParserComponent[Int, String] {
  this: OutputMapperComponent[String] =>

  override val parser: Parser = new IntParser

  class IntParser extends Parser {
    override def parse(input: Int): String = input.toString
  }
}

object ComponentRegistry
  extends IntParserComponent
  with StarOutputMapperComponent

val parser: ParserComponent[Int, String]#Parser = ComponentRegistry.parser

parser.parseAndMap(3)
