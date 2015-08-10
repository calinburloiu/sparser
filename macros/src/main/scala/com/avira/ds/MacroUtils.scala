package com.avira.ds

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
//        TypeApply(
//          Select(
//            reify(classOf).tree,
//            newTermName("apply")
//          ),
//          List(
//            TypeTree(typeOf[String])
//          )
//        )
        val className =
          if (sym.isModuleClass) {
            sym.asClass.fullName + "$"
          } else {
            sym.asClass.fullName
          }
        Literal(Constant(className))
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
}
