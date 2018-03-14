package io.getquill.idiom

import java.time.{LocalDate, LocalDateTime}

object IdiomTypes {
  implicit val stringType = SupportedType[String]
  implicit val bigDecimalType = SupportedType[BigDecimal]
  implicit val booleanType = SupportedType[Boolean]
  implicit val byteType = SupportedType[Byte]
  implicit val shortType = SupportedType[Short]
  implicit val intType = SupportedType[Int]
  implicit val longType = SupportedType[Long]
  implicit val floatType = SupportedType[Float]
  implicit val doubleType = SupportedType[Double]
  implicit val byteArrayType = SupportedType[Array[Byte]]
  implicit val localDateType = SupportedType[LocalDate]
  implicit val localDateTimeType = SupportedType[LocalDateTime]
}
