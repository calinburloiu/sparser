package com.avira.ds.sparser

sealed trait TransformResult[+T]

case class TransformSuccess[T](
    value: T)
  extends TransformResult[T]

case class TransformWarning[T](
    value: T,
    warning: ParseError)
  extends TransformResult[T]

case class TransformFailure(
    error: ParseError)
  extends TransformResult[Nothing]
