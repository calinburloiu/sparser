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
package com.avira.ds.sparser.test

import com.avira.ds.MacroUtils
import com.avira.ds.sparser._
import com.avira.ds.sparser.test.AnimalParser.{AnimalParseError, DogParseError}

sealed trait Animal {
  val family: String
  val genus: String
  val species: String
}

case class Cat(cachesMice: Boolean) extends Animal {
  override val family: String = "felidae"
  override val genus: String = "felis"
  override val species: String = "felis catus"
}

case class Dog(isHunter: Boolean) extends Animal {
  override val family: String = "canidae"
  override val genus: String = "canis"
  override val species: String = "canis lupus familiaris"
}

class AnimalParser extends Parser[String, Animal] {
  import AnimalParser._

  override protected def parse(
      initResult: ParseResult[String, String]): ParseResult[String, Animal] = {
    initResult.transform { line =>
      line.split(" ") match {
        case Array("cat", "yes") => TransformSuccess(Cat(true))
        case Array("cat", "no") => TransformSuccess(Cat(false))
        case Array("cat") => TransformWarning(Cat(true), CatParseError)
        case Array("dog", "yes") => TransformSuccess(Dog(true))
        case Array("dog", "no") => TransformSuccess(Dog(false))
        case Array("dog") => TransformWarning(Dog(true), DogParseError)
        case _ => TransformFailure(AnimalParseError)
      }
    }
  }
}

object AnimalParser {

  sealed trait GenericAnimalParseError extends ParseError {
    override val message: Option[String] = None
    override val args: Seq[Any] = Seq()
  }

  case object AnimalParseError extends GenericAnimalParseError

  case object CatParseError extends GenericAnimalParseError

  case object DogParseError extends GenericAnimalParseError

  def parseErrorClasses: Set[Class[_ <: ParseError]] =
    MacroUtils.getSealedClassChildren[GenericAnimalParseError]
        .asInstanceOf[Set[Class[_ <: ParseError]]]
}

class HierarchicalOutputParserTestSuite extends ParserTestSuite[String, Animal] {

  override val parser: Parser[String, Animal] = new AnimalParser

  ParserTest[Dog]("hunter dog",
    """dog yes""",
    { case dog: Dog => dog },
    ExpectedSuccessResult(
      ExpectedValue(
        FieldMatch(_.genus, "canis"),
        FieldMatch(_.isHunter, true)
      )
    )
  )

  ParserTest[Dog]("bad dog",
    """dog""",
    { case dog: Dog => dog },
    ExpectedWarningResult(
      ExpectedValue(
        FieldMatch(_.genus, "canis")
      ),
      ExpectedErrors(
        DogParseError.getClass
      )
    )
  )

  ParserTest[Cat]("lazy cat",
    """cat no""",
    { case cat: Cat => cat },
    ExpectedSuccessResult(
      ExpectedValue(
        FieldMatch(_.genus, "felis"),
        FieldMatch(_.cachesMice, false)
      )
    )
  )

  ParserTest("fictional animal",
    """unicorn""",
    ExpectedFailureResult(
      ExpectedErrors(
        AnimalParseError.getClass
      )
    )
  )
}
