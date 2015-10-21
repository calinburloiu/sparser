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
package com.avira.ds.etl.parsers

import com.avira.ds.MacroUtils
import com.avira.ds.sparser._
import com.fasterxml.jackson.core.JsonParseException
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

class JsonParser[O](override val conf: ParserConf)(implicit val jsonReads: Reads[O])
    extends Parser[String, O] {
  import JsonParser._

  override protected def parse(initResult: ParseResult[String, String]): ParseResult[String, O] = {
    initResult.transform { jsonString =>
      Try(
        Json.parse(jsonString).validate[O] match {
          case JsSuccess(obj, _) => TransformSuccess(obj)
          case JsError(errors) => TransformFailure(InvalidJsonParseError(errors))
        }
      ) match {
        case Success(r) => r
        case Failure(e: JsonParseException) => TransformFailure(MalformedJsonParseError(e))
        case Failure(e: Throwable) => TransformFailure(GenericJsonParseError(e))
      }
    }
  }
}

object JsonParser {
  
  sealed abstract class JsonParseError extends ParseError

  case class MalformedJsonParseError(exception: JsonParseException) extends JsonParseError {
    override val message: Option[String] = Some(exception.getMessage)
    override val args: Seq[Any] = Seq(exception)
  }

  case class GenericJsonParseError(exception: Throwable) extends JsonParseError {
    override val message: Option[String] = Some(exception.getMessage)
    override val args: Seq[Any] = Seq(exception)
  }

  case class InvalidJsonParseError(jsonErrors: Seq[(JsPath, scala.Seq[ValidationError])])
    extends JsonParseError {

    override val message: Option[String] = for {
      (_, validationErrors) <- jsonErrors.headOption
      validationError <- validationErrors.headOption
    } yield validationError.message

    override val args: Seq[Any] = jsonErrors
  }

  def parseErrorClasses: Set[Class[_ <: ParseError]] =
    MacroUtils.getSealedClassChildren[JsonParseError]
        .asInstanceOf[Set[Class[_ <: ParseError]]]
}