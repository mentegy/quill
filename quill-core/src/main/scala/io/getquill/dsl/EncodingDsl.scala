package io.getquill.dsl

import io.getquill.quotation.NonQuotedException

import scala.annotation.compileTimeOnly
import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros
import scala.language.higherKinds

trait LowPriorityImplicits {
  this: EncodingDsl =>

  implicit def materializeEncoder[T <: AnyVal]: Encoder[T] = macro EncodingDslMacro.materializeEncoder[T]

  implicit def materializeDecoder[T <: AnyVal]: Decoder[T] = macro EncodingDslMacro.materializeDecoder[T]
}

trait EncodingDsl extends LowPriorityImplicits {
  this: CoreDsl =>

  type PrepareRow
  type ResultRow

  type Index = Int

  type BaseEncoder[T] = (Index, T, PrepareRow) => PrepareRow

  type Encoder[T] <: BaseEncoder[T]

  type BaseDecoder[T] = (Index, ResultRow) => T

  type Decoder[T] <: BaseDecoder[T]

  /* ************************************************************************** */

  def lift[T](v: T): T = macro EncodingDslMacro.lift[T]

  @compileTimeOnly(NonQuotedException.message)
  def liftScalar[T](v: T)(implicit e: Encoder[T]): T = NonQuotedException()

  @compileTimeOnly(NonQuotedException.message)
  def liftCaseClass[T](v: T): T = NonQuotedException()

  /* ************************************************************************** */

  def liftQuery[U[_] <: Traversable[_], T](v: U[T]): Query[T] = macro EncodingDslMacro.liftQuery[T]

  @compileTimeOnly(NonQuotedException.message)
  def liftQueryScalar[U[_] <: Traversable[_], T](v: U[T])(implicit e: Encoder[T]): Query[T] = NonQuotedException()

  @compileTimeOnly(NonQuotedException.message)
  def liftQueryCaseClass[U[_] <: Traversable[_], T](v: U[T]): Query[T] = NonQuotedException()

  /* ************************************************************************** */

  type MappedEncoding[I, O] = io.getquill.MappedEncoding[I, O]
  val MappedEncoding = io.getquill.MappedEncoding

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I]

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O]

  protected def mappedBaseEncoder[I, O](mapped: MappedEncoding[I, O], encoder: BaseEncoder[O]): BaseEncoder[I] =
    (index, value, row) => encoder(index, mapped.f(value), row)

  protected def mappedBaseDecoder[I, O](mapped: MappedEncoding[I, O], decoder: BaseDecoder[I]): BaseDecoder[O] =
    (index, row) => mapped.f(decoder(index, row))
}


trait TraversableEncoding {
  this: EncodingDsl =>

  implicit def traversableEncoder[I, O, Col[_] <: Traversable[_]](
                                                                   implicit
                                                                   mapped: MappedEncoding[I, O],
                                                                   e:      Encoder[Col[O]],
                                                                   bf:     CanBuildFrom[Nothing, O, Col[O]]
                                                                 ): Encoder[Col[I]] = {
    mappedEncoder[Col[I], Col[O]](MappedEncoding((col: Col[I]) =>
      col.foldLeft(bf())((b, x) => b += mapped.f(x)).result()), e)
  }

  implicit def traversableDecoder[I, O, Col[_] <: Traversable[_]](
                                                                   implicit
                                                                   mapped: MappedEncoding[I, O],
                                                                   d:      Decoder[Col[I]],
                                                                   bf:     CanBuildFrom[Nothing, O, Col[O]]
                                                                 ): Decoder[Col[O]] = {
    mappedDecoder[Col[I], Col[O]](MappedEncoding((col: Col[I]) =>
      col.foldLeft(bf())((b, x) => b += mapped.f(x)).result()), d)
  }
}