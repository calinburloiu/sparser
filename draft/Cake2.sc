trait OutputMapperComponent[O] {

  val outputMapper: OutputMapper

  trait OutputMapper {
    def map(output: O): O
  }
}

trait StarOutputMapperComponent extends OutputMapperComponent[String] {
  override val outputMapper: OutputMapper = new StarOutputMapper

  class StarOutputMapper extends OutputMapper {
    override def map(output: String): String = s"*$output"
  }
}

trait Parser[I, O] {
  this: OutputMapperComponent[O] =>

  def parse(input: I): O

  def parseAndMap(input: I) = outputMapper.map(parse(input))
}

class IntParser extends Parser[Int, String] {
  this: OutputMapperComponent[String] =>

  override def parse(input: Int): String = input.toString
}

val parser = new IntParser with StarOutputMapperComponent

parser.parseAndMap(3)