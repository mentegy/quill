package io.getquill.dsl

import scala.language.experimental.macros
import scala.language.higherKinds

trait LowPriorityImplicits {
  this: EncodingDsl =>

  implicit def anyValEncoder[T <: AnyVal]: Encoder[T] = macro EncodingDslMacro.anyValEncoder[T]

  implicit def anyValDecoder[T <: AnyVal]: Decoder[T] = macro EncodingDslMacro.anyValDecoder[T]
}

trait EncodingDsl extends LowPriorityImplicits {

  type PrepareRow
  type ResultRow

  type Index = Int

  type BaseEncoder[T] = (Index, T, PrepareRow) => PrepareRow

  type Encoder[T] <: BaseEncoder[T]

  type BaseDecoder[T] = (Index, ResultRow) => T

  type Decoder[T] <: BaseDecoder[T]

  type MappedEncoding[I, O] = io.getquill.MappedEncoding[I, O]
  val MappedEncoding = io.getquill.MappedEncoding

  implicit def anyValMappedEncoder[I <: AnyVal, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I] = mappedEncoder

  implicit def anyValMappedDecoder[I, O <: AnyVal](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O] = mappedDecoder

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I]

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O]

  protected def mappedBaseEncoder[I, O](mapped: MappedEncoding[I, O], encoder: BaseEncoder[O]): BaseEncoder[I] =
    (index, value, row) => encoder(index, mapped.f(value), row)

  protected def mappedBaseDecoder[I, O](mapped: MappedEncoding[I, O], decoder: BaseDecoder[I]): BaseDecoder[O] =
    (index, row) => mapped.f(decoder(index, row))
}
