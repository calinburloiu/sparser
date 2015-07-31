// Get type as parent Parser[I, O].
res1.tpe.baseType(res1.tpe.baseClasses(1))

// Nested declarations.
getTypeTag(n).tpe
	.declaration(ru.newTermName("c")).asTerm.typeSignature
		.declaration(ru.newTermName("x")).asTerm

def cast2[T : ru.TypeTag](obj: Any, tag: ru.TypeTag[T]) = obj.asInstanceOf[T]
val an: Any = NestedObject(...)
cast2(an, tn)