package io.getquill.context.sql

import java.sql.Timestamp
import java.time._
import java.util.{ Date, UUID }

import io.getquill._
import io.getquill.context.mirror.Row
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.sql.testContext._

import scala.util.Try
import io.getquill.context.Context
import io.getquill.context.sql.idiom.ConcatSupport

class SqlContextSpec extends Spec {

  "binds inputs according to the sql terms order" - {
    "filter.update" in {
      val q = quote {
        qr1.filter(t => t.i == lift(1)).update(t => t.l -> lift(2L))
      }
      val mirror = testContext.run(q)
      mirror.string mustEqual "UPDATE TestEntity SET l = ? WHERE i = ?"
      mirror.prepareRow mustEqual Row(2l, 1)
    }
    "filter.map" in {
      val q = quote {
        qr1.filter(t => t.i == lift(1)).map(t => lift(2L))
      }
      val mirror = testContext.run(q)
      mirror.string mustEqual "SELECT ? FROM TestEntity t WHERE t.i = ?"
      mirror.prepareRow mustEqual Row(2l, 1)
    }
  }

  "fails if the sql dialect is not valid" in {

    "testContext.run(qr1.delete)" mustNot compile

    class EvilDBDialect extends SqlIdiom with ConcatSupport {
      override def liftingPlaceholder(index: Int): String = "?"

      override def prepareForProbing(string: String) = string
    }
    object testContext extends Context[MirrorSqlDialect, Literal] with SqlContext[MirrorSqlDialect, Literal] {

      val idiom = MirrorSqlDialect
      val naming = Literal

      override type PrepareRow = List[Any]
      override type ResultRow = List[Any]

      type Encoder[T] = BaseEncoder[T]
      type Decoder[T] = BaseDecoder[T]

      override def close = ()

      def probe(sql: String): Try[Any] = null

      def encoder[T]: Encoder[T] = (index: Index, value: T, row: PrepareRow) => row

      def decoder[T]: Decoder[T] = (index: Index, row: ResultRow) => row(index).asInstanceOf[T]

      implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]] = encoder[Option[T]]

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
      implicit val dateEncoder: Encoder[Date] = encoder[Date]
      implicit val localDateEncoder: Encoder[LocalDate] = encoder[LocalDate]
      implicit val uuidEncoder: Encoder[UUID] = encoder[UUID]

      implicit def optionDecoder[T](implicit d: Decoder[T]): Decoder[Option[T]] = decoder[Option[T]]

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
      implicit val dateDecoder: Decoder[Date] = decoder[Date]
      implicit val uuidDecoder: Decoder[UUID] = decoder[UUID]

      implicit val timestampDecoder: Decoder[Timestamp] = decoder[Timestamp]
      implicit val timestampEncoder: Encoder[Timestamp] = encoder[Timestamp]
      implicit val localDateTimeDecoder: Decoder[LocalDateTime] = decoder[LocalDateTime]
      implicit val localDateTimeEncoder: Encoder[LocalDateTime] = encoder[LocalDateTime]
      implicit val localTimeDecoder: Decoder[LocalTime] = decoder[LocalTime]
      implicit val localTimeEncoder: Encoder[LocalTime] = encoder[LocalTime]
      implicit val instantDecoder: Decoder[Instant] = decoder[Instant]
      implicit val instantEncoder: Encoder[Instant] = encoder[Instant]
      implicit val zonedDateTimeDecoder: Decoder[ZonedDateTime] = decoder[ZonedDateTime]
      implicit val zonedDateTimeEncoder: Encoder[ZonedDateTime] = encoder[ZonedDateTime]
      implicit val offsetDateTimeDecoder: Decoder[OffsetDateTime] = decoder[OffsetDateTime]
      implicit val offsetDateTimeEncoder: Encoder[OffsetDateTime] = encoder[OffsetDateTime]
      implicit val offsetTimeDecoder: Decoder[OffsetTime] = decoder[OffsetTime]
      implicit val offsetTimeEncoder: Encoder[OffsetTime] = encoder[OffsetTime]

      implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], e: Encoder[O]): Encoder[I] =
        encoder[I]

      implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], d: Decoder[I]): Decoder[O] =
        decoder[O]
    }
  }
}
