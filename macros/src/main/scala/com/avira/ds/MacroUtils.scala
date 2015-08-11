package com.avira.ds

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.macros.Context

object MacroUtils {

  def getSealedClassChildren[C]: Set[Class[_]] = macro getSealedClassChildrenImpl[C]

  def getSealedClassChildrenImpl[C: c.WeakTypeTag](c: Context): c.Expr[Set[Class[_]]] = {
    import c.universe._

    val symbol = weakTypeOf[C].typeSymbol

    if (!symbol.isClass) {
      c.abort(
        c.enclosingPosition,
        "Symbol is not a class or trait."
      )
    } else if (!symbol.asClass.isSealed) {
      c.abort(
        c.enclosingPosition,
        "Class or trait is not sealed."
      )
    } else {
      val childrenNames = symbol.asClass.knownDirectSubclasses.toList.map { sym =>
        Literal(Constant(symbolToClassName(c)(sym)))
      }

      val childrenClasses = childrenNames.map { child =>
        reify {
          Class.forName(c.Expr[String](child).splice)
        }.tree
      }

      c.Expr[Set[Class[_]]] {
        Apply(
          Select(
            reify(Set).tree,
            newTermName("apply")
          ),
          childrenClasses
        )
      }
    }
  }

  private def symbolToClassName(c: Context)(symbol: c.universe.Symbol): String = {
    @tailrec
    def fixForSubclasses(acc: String, components: List[String]): String = components match {
      case x0 :: x1 :: xs if x0(0).isUpper =>
        fixForSubclasses(acc + x0 + "$", x1 :: xs)
      case x :: Nil => acc + x
      case x :: xs =>
        fixForSubclasses(acc + x + ".", xs)
    }

    /* In order to be able to load a class, the name requires $ separator between classes and
    subclasses and an extra $ at the end if the symbol represent a Scala object. rawName contains
    only dots like in code.
     */
    val rawName = symbol.asClass.fullName
    val nameWithSubclassesFixed = fixForSubclasses("", rawName.split("[.]").toList)

    if (symbol.isModuleClass) {
      nameWithSubclassesFixed + "$"
    } else {
      nameWithSubclassesFixed
    }
  }
}
