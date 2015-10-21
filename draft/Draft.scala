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
import scala.util.parsing.combinator.testing.Ident

// Get type as parent Parser[I, O].
res1.tpe.baseType(res1.tpe.baseClasses(1))

// Nested declarations.
getTypeTag(n).tpe
	.declaration(ru.newTermName("c")).asTerm.typeSignature
		.declaration(ru.newTermName("x")).asTerm

def cast2[T : ru.TypeTag](obj: Any, tag: ru.TypeTag[T]) = obj.asInstanceOf[T]
val an: Any = NestedObject(...)
cast2(an, tn)

def getCCParams(cc: AnyRef): Map[String, Any] =
  (Map[String, Any]() /: cc.getClass.getDeclaredFields) {(a, f) =>
    f.setAccessible(true)
    val value = f.get(cc) match {
      // this covers tuples as well as case classes, so there may be a more specific way
      case caseClassInstance: Product => getCCParams(caseClassInstance)
      case x => x
    }
    a + (f.getName -> value)
  }

Function(
  List(
    ValDef(
      Modifiers(PARAM | SYNTHETIC),
      newTermName("x$1"),
      TypeTree(),
      EmptyTree
    )
  ),
  Select(
    Ident(newTermName("x$1")),
    newTermName("x")
  )
)

case class Res(x: Int, y: String, z: Z)
case class Z(a: String, b: Float)

def func_impl(c: Context)(f: c.Expr[Res => Any]): c.Expr[Unit] = {
  import c.universe._

  println(showRaw(f.tree))
  println(show(f.tree))

  val params = f.tree match {
    case Function(_, s) =>
      println(show(s))
      // println(showRaw(t))
      s.toString
  }

  // FIXME Contains free term variable params!
  // val expr = reify { println(c.params) }

  // c.Expr[Unit](Block(List(
    // Apply(Select(Ident(scala.Predef), newTermName("println")), List(Literal(Constant(params))))
    // ), Literal(Constant(()))))
  ???
}

def func(f: Res => Any): Unit = macro func_impl
