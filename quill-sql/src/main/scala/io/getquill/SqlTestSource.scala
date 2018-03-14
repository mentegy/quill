package io.getquill

import java.time.{LocalDate, LocalDateTime}

import io.getquill.context.mirror.Row
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.source.{ScalaFutureIOSource, Source}

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

trait SqlTestSource[I <: SqlIdiom[_]] extends Source[I] with SqlTestSourceEncoders with SqlTestSourceDecoders {
  type PrepareRow = Row
  type ResultRow = Row
}

class SqlTestSyncSource[I <: SqlIdiom[_]] extends SqlTestSource[I] {
  type Result[T] = T

  def executeQuery[T](string: String, prepare: Prepare, extractor: Extractor[T]): List[T] = {
    println(s"executing sync:")
    println(string)
    println(prepare)
    println(extractor)
    Nil
  }
}

class SqlTestAsyncSource[I <: SqlIdiom[_]] extends SqlTestSource[I] with ScalaFutureIOSource {

  def executeQuery[T](string: String, prepare: Prepare, extractor: Extractor[T])(implicit ec: ExecutionContext): Future[List[T]] = {
    println(s"executing async:")
    println(string)
    println(prepare)
    println(extractor)
    Future(Nil)
  }
}

trait SqlTestSourceEncoders {
  this: SqlTestSource[_] =>

  type Encoder[T] = MirrorEncoder[T]

  case class MirrorEncoder[T](encoder: BaseEncoder[T]) extends BaseEncoder[T] {
    override def apply(index: Index, value: T, row: PrepareRow) =
      encoder(index, value, row)
  }

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], e: MirrorEncoder[O]): MirrorEncoder[I] =
    MirrorEncoder((index: Index, value: I, row: PrepareRow) => e(index, mapped.f(value), row))

  def encoder[T]: Encoder[T] = MirrorEncoder((index: Index, value: T, row: PrepareRow) => row.add(value))

  implicit val stringEncoder: Encoder[String] = encoder[String]
  implicit val bigDecimalEncoder: Encoder[BigDecimal] = encoder[BigDecimal]
  implicit val booleanEncoder: Encoder[Boolean] = encoder[Boolean]
  implicit val byteEncoder: Encoder[Byte] = encoder[Byte]
  implicit val shortEncoder: Encoder[Short] = encoder[Short]
  implicit val intEncoder: Encoder[Int] = encoder[Int]
  implicit val longEncoder: Encoder[Long] = encoder[Long]
  implicit val floatEncoder: Encoder[Float] = encoder[Float]
  implicit val doubleEncoder: Encoder[Double] = encoder[Double]
  implicit val byteArrayEncoder: Encoder[Array[Byte]] = encoder[Array[Byte]]
  implicit val localDateEncoder: Encoder[LocalDate] = encoder[LocalDate]
  implicit val localDateTimeEncoder: Encoder[LocalDateTime] = encoder[LocalDateTime]

}

trait SqlTestSourceDecoders {
  this: SqlTestSource[_] =>

  type Decoder[T] = MirrorDecoder[T]

  case class MirrorDecoder[T](decoder: BaseDecoder[T]) extends BaseDecoder[T] {
    override def apply(index: Index, row: ResultRow) =
      decoder(index, row)
  }

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], d: MirrorDecoder[I]): MirrorDecoder[O] =
    MirrorDecoder((index: Index, row: ResultRow) => mapped.f(d.apply(index, row)))

  def decoder[T: ClassTag]: Decoder[T] = MirrorDecoder((index: Index, row: ResultRow) => row[T](index))

  implicit val stringDecoder: Decoder[String] = decoder[String]
  implicit val bigDecimalDecoder: Decoder[BigDecimal] = decoder[BigDecimal]
  implicit val booleanDecoder: Decoder[Boolean] = decoder[Boolean]
  implicit val byteDecoder: Decoder[Byte] = decoder[Byte]
  implicit val shortDecoder: Decoder[Short] = decoder[Short]
  implicit val intDecoder: Decoder[Int] = decoder[Int]
  implicit val longDecoder: Decoder[Long] = decoder[Long]
  implicit val floatDecoder: Decoder[Float] = decoder[Float]
  implicit val doubleDecoder: Decoder[Double] = decoder[Double]
  implicit val byteArrayDecoder: Decoder[Array[Byte]] = decoder[Array[Byte]]
  implicit val localDateDecoder: Decoder[LocalDate] = decoder[LocalDate]
  implicit val localDateTimeDecoder: Decoder[LocalDateTime] = decoder[LocalDateTime]
}