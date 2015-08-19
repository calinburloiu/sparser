package com.avira.ds

/** Main package of SParser project where all core classes are located.
  *
  * The main trait is [[com.avira.ds.sparser.Parser]] which can be extended to
  * transform any type of input in a desired value wrapped into a
  * [[com.avira.ds.sparser.ParseResult]] object which collects errors as
  * [[com.avira.ds.sparser.ParseError]] child class instances.
  *
  * The parsing process can be divided into one or more monadic operations on
  * [[com.avira.ds.sparser.ParseResult]] objects by using `transform` method
  * which accepts a `flatMap`-like lambda function. The return type of this
  * function is an instance of [[com.avira.ds.sparser.TransformResult]] which
  * has cases for success and failure and can encapsulate an error.
  */
package object sparser {

}
