package io.getquill.idiom

// DEV API
trait SupportedType[T]
object SupportedType {
  def apply[T]: SupportedType[T] = new SupportedType[T] {}
}
