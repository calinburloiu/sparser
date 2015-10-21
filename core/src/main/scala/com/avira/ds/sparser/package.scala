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
