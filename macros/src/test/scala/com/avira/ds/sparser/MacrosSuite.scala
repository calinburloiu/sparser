package com.avira.ds.sparser

import com.avira.ds.MacroUtils
import com.avira.ds.sparser.FieldMatch._
import org.scalatest.WordSpec

case class Xx(x1: String, xMore: Option[Yy])
case class Yy(y1: Int, y2: String, yMore: Seq[Zz])
case class Zz(z1: Float)

class MacrosSuite extends WordSpec {

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
