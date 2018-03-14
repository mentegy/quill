package io.getquill.source

import java.time.{ LocalDate, LocalDateTime }

import io.getquill.dsl.EncodingDsl
import io.getquill.idiom.Idiom
import io.getquill.monad.IO

import scala.language.experimental.macros
import scala.language.higherKinds

trait Source[I <: Idiom[_]] extends EncodingDsl {

  type Result[T]
  type Prepare = PrepareRow => (List[Any], PrepareRow)
  type Extractor[T] = ResultRow => T

  def apply[T](io: IO[T]): Result[T] = macro SourceMacro.expandApply[T]

  implicit val stringDecoder: Decoder[String]
  implicit val bigDecimalDecoder: Decoder[BigDecimal]
  implicit val booleanDecoder: Decoder[Boolean]
  implicit val byteDecoder: Decoder[Byte]
  implicit val shortDecoder: Decoder[Short]
  implicit val intDecoder: Decoder[Int]
  implicit val longDecoder: Decoder[Long]
  implicit val floatDecoder: Decoder[Float]
  implicit val doubleDecoder: Decoder[Double]
  implicit val byteArrayDecoder: Decoder[Array[Byte]]
  implicit val localDateDecoder: Decoder[LocalDate]
  implicit val localDateTimeDecoder: Decoder[LocalDateTime]

  implicit val stringEncoder: Encoder[String]
  implicit val bigDecimalEncoder: Encoder[BigDecimal]
  implicit val booleanEncoder: Encoder[Boolean]
  implicit val byteEncoder: Encoder[Byte]
  implicit val shortEncoder: Encoder[Short]
  implicit val intEncoder: Encoder[Int]
  implicit val longEncoder: Encoder[Long]
  implicit val floatEncoder: Encoder[Float]
  implicit val doubleEncoder: Encoder[Double]
  implicit val byteArrayEncoder: Encoder[Array[Byte]]
  implicit val localDateEncoder: Encoder[LocalDate]
  implicit val localDateTimeEncoder: Encoder[LocalDateTime]


  protected case class Run[T](f: () => Result[T])
}