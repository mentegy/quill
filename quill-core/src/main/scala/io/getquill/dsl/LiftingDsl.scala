package io.getquill.dsl

import io.getquill.quotation.NonQuotedException

import scala.annotation.compileTimeOnly
import scala.language.higherKinds

trait LiftingDsl {
  this: CoreDsl =>

  def lift[T](v: T): T = NonQuotedException()

  @compileTimeOnly(NonQuotedException.message)
  def liftScalar[T](v: T): T = NonQuotedException()

  @compileTimeOnly(NonQuotedException.message)
  def liftCaseClass[T](v: T): T = NonQuotedException()

  /* ************************************************************************** */

  def liftQuery[U[_] <: Traversable[_], T](v: U[T]): Query[T] = NonQuotedException()

  @compileTimeOnly(NonQuotedException.message)
  def liftQueryScalar[U[_] <: Traversable[_], T](v: U[T]): Query[T] = NonQuotedException()

  @compileTimeOnly(NonQuotedException.message)
  def liftQueryCaseClass[U[_] <: Traversable[_], T](v: U[T]): Query[T] = NonQuotedException()
}
