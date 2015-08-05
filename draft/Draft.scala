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
