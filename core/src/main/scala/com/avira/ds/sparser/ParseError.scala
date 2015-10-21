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
 * Abstracts an error returned by a [[Parser]] implementation.
 *
 * Parser developers are recommended to extend this trait with a sealed
 * abstract class or trait as a parent for all specific errors returnable by a
 * parser. A series of case classes/objects can represent all kinds of errors
 * required. [[Parser]] users can then use pattern matching to act on different
 * kind of errors. Abstract members `message` and `args`, as well as method
 * `name`, provide a common interface to errors.
 *
 * The optional `message` should provide a human readable description of the
 * problem that gives enough information for understanding whether the data is
 * invalid or the [[Parser]] implementation needs to be updated.
 *
 * The `args` member is a free list of arguments which provides technical
 * details about the error. Conventionally, if the error was triggered by an
 * Exception, the first argument should be the exception thrown. Here are some
 * examples of how to use `args` for detailing errors:
 *  - While splitting a CSV line, the parser encountered an unexpected number
 *  of columns. The first element is the actual number of columns found.
 *  - An integer could not be parsed. Either the `NumberFormatException` can be
 *  added in `args` or the actual field which couldn't be parsed as integer.
 */
trait ParseError extends Serializable {

  /** A human readable description of the error (e.g. "Invalid age xyz, you
    *  should provide a positive integer").
    */
  val message: Option[String]

  /** Sequence of objects which might help while debugging the error */
  val args: Seq[Any]

  /** Returns an unique name for the parser (FQN of class). */
  def name: String = this.getClass.getName

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

  override def toString: String = {
    s"""{"name": "$name", "message": "${message.getOrElse("")}", "args": ${args.mkString("[\"", "\", \"", "\"]")}}""" // scalastyle:ignore
  }
}
