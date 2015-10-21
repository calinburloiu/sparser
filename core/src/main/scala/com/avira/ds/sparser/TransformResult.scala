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

/** Type returned by the lambda function accepted by [[ParseResult]]'s `transform`
  * method which may contain a value and an error.
  *
  * The following case classes extend this sealed trait:
  *
  *  - [[TransformSuccess]]: contains a value and no error;
  *  - [[TransformWarning]]: contains a (potentially incomplete) value and an error;
  *  - [[TransformFailure]]: contains no value and an error.
  *
  * @tparam T Type of the value which might be contained by this
  * @param valueOption Value that might be contained by this
  * @param errorOption Error that might be contained by this
  */
sealed abstract class TransformResult[+T](
  val valueOption: Option[T],
  val errorOption: Option[ParseError])

/** Child of [[TransformResult]] which contains a value and no error.
  */
case class TransformSuccess[T](
    value: T)
  extends TransformResult(Some(value), None)

/** Child of [[TransformResult]] which contains a (potentially incomplete)
  * value and an error (a.k.a. warning).
  */
case class TransformWarning[T](
    value: T,
    warning: ParseError)
  extends TransformResult(Some(value), Some(warning))

/** Child of [[TransformResult]] which contains no value and an error.
  */
case class TransformFailure(
    error: ParseError)
  extends TransformResult(None, Some(error))
