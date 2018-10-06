package io.getquill.context.jdbc

import java.sql
import java.sql.{ Timestamp, Types }
import java.time._
import java.util.{ Calendar, Date }

import io.getquill.context.DateEncoding

trait JdbcDateEncoding extends DateEncoding {
  this: JdbcContext[_, _] =>

  implicit val dateDecoder: Decoder[Date] = getTimestamp(t => new Date(t.getTime))
  implicit val dateEncoder: Encoder[Date] = setTimestamp(v => new Timestamp(v.getTime))

  implicit val timestampDecoder: Decoder[Timestamp] = getTimestamp(identity)
  implicit val timestampEncoder: Encoder[Timestamp] = setTimestamp(identity)

  implicit val localDateDecoder: Decoder[LocalDate] = decoder((i, r) => r.getDate(i, calendar()).toLocalDate)
  implicit val localDateEncoder: Encoder[LocalDate] =
    encoder(Types.DATE, (i, v, r) => r.setDate(i, sql.Date.valueOf(v), calendar()))

  implicit val localDateTimeDecoder: Decoder[LocalDateTime] = getTimestamp(_.toLocalDateTime)
  implicit val localDateTimeEncoder: Encoder[LocalDateTime] = setTimestamp(Timestamp.valueOf)

  implicit val localTimeDecoder: Decoder[LocalTime] = decoder((i, r) => r.getTime(i, calendar()).toLocalTime)
  implicit val localTimeEncoder: Encoder[LocalTime] =
    encoder(Types.TIME, (i, v, r) => r.setTime(i, sql.Time.valueOf(v), calendar()))

  implicit val instantDecoder: Decoder[Instant] = getTimestamp(_.toInstant)
  implicit val instantEncoder: Encoder[Instant] = setTimestamp(Timestamp.from)

  implicit val zonedDateTimeDecoder: Decoder[ZonedDateTime] = getTimestamp(v => v.toInstant.atZone(dateTimeZone.toZoneId))
  implicit val zonedDateTimeEncoder: Encoder[ZonedDateTime] = setTimestamp(v => Timestamp.from(v.toInstant))

  implicit val offsetDateTimeDecoder: Decoder[OffsetDateTime] =
    getTimestamp(v => OffsetDateTime.ofInstant(v.toInstant, dateTimeZone.toZoneId))
  implicit val offsetDateTimeEncoder: Encoder[OffsetDateTime] =
    setTimestamp(v => Timestamp.from(v.toInstant))

  implicit val offsetTimeDecoder: Decoder[OffsetTime] =
    decoder((i, r) => r.getTime(i, calendar()).toLocalTime.atOffset(ZoneOffset.of(dateTimeZone.getID)))
  implicit val offsetTimeEncoder: Encoder[OffsetTime] =
    encoder(Types.TIME, (i, v, r) =>
      r.setTime(i, sql.Time.valueOf(v.toLocalTime.plusSeconds(v.getOffset.getTotalSeconds.toLong)), calendar()))

  private def calendar() = Calendar.getInstance(dateTimeZone)
  private def getTimestamp[T](f: Timestamp => T): Decoder[T] = decoder((i, r) => f(r.getTimestamp(i, calendar())))
  private def setTimestamp[T](f: T => Timestamp): Encoder[T] =
    encoder(Types.TIMESTAMP, (i, v, r) => r.setTimestamp(i, f(v), calendar()))
}
