package com.avira.ds

trait Felina[+T] {
  val x: T
}

sealed abstract class Pisica[T](override val x: T) extends Felina[T]

object Draft {

  def main(args: Array[String]): Unit = {

  }
}
