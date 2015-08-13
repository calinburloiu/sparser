package com.avira.ds.sparser

/**
 * Created by calinburloiu on 13/08/15.
 */
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
