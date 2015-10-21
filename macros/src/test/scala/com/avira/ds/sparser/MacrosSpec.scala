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

import com.avira.ds.MacroUtils
import com.avira.ds.sparser.test.FieldMatch
import com.avira.ds.sparser.test.FieldMatch._
import org.scalatest.WordSpec

case class Xx(x1: String, xMore: Option[Yy])
case class Yy(y1: Int, y2: String, yMore: Seq[Zz])
case class Zz(z1: Float)

/** Test suite for testing macros. */
class MacrosSpec extends WordSpec {

  "selectFieldToString" should {
    "convert to String a simple first-level field selector" in {
      assert(selectFieldToString[Xx](_.x1) === "x1")
    }

    "convert to String a nested selector with get and apply" in {
      assert(selectFieldToString[Xx](_.xMore.get.yMore(1).z1) === "xMore.get.yMore(1).z1")
    }
  }

  "Calling FieldMatch.apply without fieldName" when {
    "a simple first-level field selector is passed" should {
      "create a FieldMatch with fieldName" in {
        val fm = FieldMatch[Xx]({ xx: Xx => xx.x1 }, "value")
        assert(fm.fieldName == "x1")
      }
    }

    "a nested selector with get and apply" should {
      "create a FieldMatch with fieldName" in {
        val fm = FieldMatch[Xx]({ xx: Xx => xx.xMore.get.yMore(1).z1 }, "value")
        assert(fm.fieldName == "xMore.get.yMore(1).z1")
      }
    }
  }

  "getSealedClassChildren" should {
    "get all subclasses of SamplePersonParseError" in {
        val classes = MacroUtils.getSealedClassChildren[Option[String]]
        assert(classes.contains(classOf[Some[String]]))
        assert(classes.contains(None.getClass))
    }
  }
}

