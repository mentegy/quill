package io.getquill.context.mirror

import java.sql.Timestamp
import java.time._
import java.util.Date

import io.getquill.MirrorContext
import io.getquill.context.DateEncoding

trait MirrorDateEncoding extends DateEncoding {
  this: MirrorContext[_, _] =>

  implicit val dateDecoder: Decoder[Date] = decoder[Date]
  implicit val dateEncoder: Encoder[Date] = encoder[Date]

  implicit val timestampDecoder: Decoder[Timestamp] = decoder[Timestamp]
  implicit val timestampEncoder: Encoder[Timestamp] = encoder[Timestamp]

  implicit val localDateDecoder: Decoder[LocalDate] = decoder[LocalDate]
  implicit val localDateEncoder: Encoder[LocalDate] = encoder[LocalDate]

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
}
