package io.getquill.context

import java.sql.Timestamp
import java.time._
import java.util.{ Date, TimeZone }

trait DateEncoding {
  this: Context[_, _] =>

  protected lazy val dateTimeZone: TimeZone = TimeZone.getDefault

  implicit val dateDecoder: Decoder[Date]
  implicit val dateEncoder: Encoder[Date]

  implicit val timestampDecoder: Decoder[Timestamp]
  implicit val timestampEncoder: Encoder[Timestamp]

  implicit val localDateDecoder: Decoder[LocalDate]
  implicit val localDateEncoder: Encoder[LocalDate]

  implicit val localDateTimeDecoder: Decoder[LocalDateTime]
  implicit val localDateTimeEncoder: Encoder[LocalDateTime]

  implicit val localTimeDecoder: Decoder[LocalTime]
  implicit val localTimeEncoder: Encoder[LocalTime]

  implicit val instantDecoder: Decoder[Instant]
  implicit val instantEncoder: Encoder[Instant]

  implicit val zonedDateTimeDecoder: Decoder[ZonedDateTime]
  implicit val zonedDateTimeEncoder: Encoder[ZonedDateTime]

  implicit val offsetDateTimeDecoder: Decoder[OffsetDateTime]
  implicit val offsetDateTimeEncoder: Encoder[OffsetDateTime]

  implicit val offsetTimeDecoder: Decoder[OffsetTime]
  implicit val offsetTimeEncoder: Encoder[OffsetTime]
}
