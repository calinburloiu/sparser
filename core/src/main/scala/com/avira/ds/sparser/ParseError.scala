package com.avira.ds.sparser

/**
 * Abstracts an error returned by a [[com.avira.ds.sparser.Parser]] implementation.
 */
trait ParseError extends Serializable {

  /** A more detailed description of the error
    * (e.g. "Invalid age xyz, you should provide a positive integer")
    */
  val message: Option[String]

  /** Sequence of object which might help while debugging the error */
  val args: Seq[Any]

  def name: String = this.getClass.getCanonicalName

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
