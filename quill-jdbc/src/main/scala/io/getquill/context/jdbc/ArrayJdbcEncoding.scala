package io.getquill.context.jdbc

import java.sql.{JDBCType, Types}
import java.time.LocalDate
import java.util.{Date, UUID}

import io.getquill.context.sql.dsl.ArrayEncoding

import scala.collection.generic.CanBuildFrom

trait ArrayJdbcEncoding extends ArrayEncoding {
  self: JdbcContext[_, _] =>

  implicit def arrayStringEncoder[Col <: Traversable[String]]: Encoder[Col] = rawEncoder[String, Col](Types.VARCHAR)
  implicit def arrayBigDecimalEncoder[Col <: Traversable[BigDecimal]]: Encoder[Col] = rawEncoder[BigDecimal, Col](Types.NUMERIC)
  implicit def arrayBooleanEncoder[Col <: Traversable[Boolean]]: Encoder[Col] = rawEncoder[Boolean, Col](Types.BOOLEAN)
  implicit def arrayByteEncoder[Col <: Traversable[Byte]]: Encoder[Col] = rawEncoder[Byte, Col](Types.TINYINT)
  implicit def arrayShortEncoder[Col <: Traversable[Short]]: Encoder[Col] = rawEncoder[Short, Col](Types.SMALLINT)
  implicit def arrayIntEncoder[Col <: Traversable[Int]]: Encoder[Col] = rawEncoder[Int, Col](Types.INTEGER)
  implicit def arrayLongEncoder[Col <: Traversable[Long]]: Encoder[Col] = rawEncoder[Long, Col](Types.BIGINT)
  implicit def arrayFloatEncoder[Col <: Traversable[Float]]: Encoder[Col] = rawEncoder[Float, Col](Types.FLOAT)
  implicit def arrayDoubleEncoder[Col <: Traversable[Double]]: Encoder[Col] = rawEncoder[Double, Col](Types.DOUBLE)
  implicit def arrayArrayEncoder[Col <: Traversable[Array[Byte]]]: Encoder[Col] = rawEncoder[Array[Byte], Col](Types.VARBINARY)
  implicit def arrayDateEncoder[Col <: Traversable[Date]]: Encoder[Col] = rawEncoder[Date, Col](Types.TIMESTAMP)
  implicit def arrayLocalDateEncoder[Col <: Traversable[LocalDate]]: Encoder[Col] = rawEncoder[LocalDate, Col](Types.DATE)

  implicit def arrayStringDecoder[Col <: Traversable[String]](implicit bf: CanBuildFrom[Nothing, String, Col]): Decoder[Col] = rawDecoder[String, Col](Types.VARCHAR)
  implicit def arrayBigDecimalDecoder[Col <: Traversable[BigDecimal]](implicit bf: CBF[BigDecimal, Col]): Decoder[Col] = rawDecoder[BigDecimal, Col](Types.NUMERIC)
  implicit def arrayBooleanDecoder[Col <: Traversable[Boolean]](implicit bf: CBF[Boolean, Col]): Decoder[Col] = rawDecoder[Boolean, Col](Types.BOOLEAN)
  implicit def arrayByteDecoder[Col <: Traversable[Byte]](implicit bf: CBF[Byte, Col]): Decoder[Col] = rawDecoder[Byte, Col](Types.TINYINT)
  implicit def arrayShortDecoder[Col <: Traversable[Short]](implicit bf: CBF[Short, Col]): Decoder[Col] = rawDecoder[Short, Col](Types.SMALLINT)
  implicit def arrayIntDecoder[Col <: Traversable[Int]](implicit bf: CBF[Int, Col]): Decoder[Col] = rawDecoder[Int, Col](Types.INTEGER)
  implicit def arrayLongDecoder[Col <: Traversable[Long]](implicit bf: CBF[Long, Col]): Decoder[Col] = rawDecoder[Long, Col](Types.BIGINT)
  implicit def arrayFloatDecoder[Col <: Traversable[Float]](implicit bf: CBF[Float, Col]): Decoder[Col] = rawDecoder[Float, Col](Types.FLOAT)
  implicit def arrayDoubleDecoder[Col <: Traversable[Double]](implicit bf: CBF[Double, Col]): Decoder[Col] = rawDecoder[Double, Col](Types.DOUBLE)
  implicit def arrayArrayDecoder[Col <: Traversable[Array[Byte]]](implicit bf: CBF[Array[Byte], Col]): Decoder[Col] = rawDecoder[Array[Byte], Col](Types.VARBINARY)
  implicit def arrayDateDecoder[Col <: Traversable[Date]](implicit bf: CBF[Date, Col]): Decoder[Col] = rawDecoder[Date, Col](Types.TIMESTAMP)
  implicit def arrayLocalDateDecoder[Col <: Traversable[LocalDate]](implicit bf: CBF[LocalDate, Col]): Decoder[Col] = rawDecoder[LocalDate, Col](Types.DATE)


  private def rawEncoder[T, Col <: Traversable[T]](jdbcType: Int): Encoder[Col] =
    arrayEncoder[T, Col](jdbcType, _.asInstanceOf[AnyRef])

  private def rawDecoder[T, Col <: Traversable[T]](jdbcType: Int)(implicit bf: CanBuildFrom[Nothing, T, Col]): Decoder[Col] =
    arrayDecoder[T, T, Col](jdbcType, identity)

  protected def parseJdbcType(intType: Int): String = JDBCType.valueOf(intType).getName

  def arrayEncoder[T, Col <: Traversable[T]](jdbcType: Int, mapper: T => AnyRef): Encoder[Col] = {
    encoder[Col](Types.ARRAY, (idx: Index, seq: Col, row: PrepareRow) => {
      val bf = implicitly[CanBuildFrom[Nothing, AnyRef, Array[AnyRef]]]
      row.setArray(
        idx,
        withConnection(_.createArrayOf(
          parseJdbcType(jdbcType),
          seq.foldLeft(bf())((b, x) => b += mapper(x)).result()
        ))
      )
    })
  }

  def arrayDecoder[I, O, Col <: Traversable[O]](jdbcType: Int, mapper: I => O)(implicit bf: CanBuildFrom[Nothing, O, Col]): Decoder[Col] = {
    decoder[Col](Types.ARRAY, (idx: Index, row: ResultRow) => {
      val arr = row.getArray(idx)
      if (arr == null) bf().result()
      else {
        val resType = arr.getBaseType
        require(
          resType == jdbcType || parseJdbcType(arr.getBaseType) == parseJdbcType(resType),
          s"Retrieved unsupported jdbc type: ${parseJdbcType(jdbcType)}, expected: ${arr.getBaseTypeName}"
        )
        arr.getArray.asInstanceOf[Array[I]]
          .foldLeft(bf())((b, x) => b += mapper(x))
          .result()
      }
    })
  }
}
