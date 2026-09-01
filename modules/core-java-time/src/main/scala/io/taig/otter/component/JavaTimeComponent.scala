package io.taig.otter.component

import cats.syntax.all.*
import io.taig.otter.operation.PrimitiveOperation

import java.time.DateTimeException
import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.MonthDay
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.time.Period
import java.time.Year
import java.time.YearMonth
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

/** The `java.time` vocabulary, carried as text.
  *
  * Format agnostic: it asks only for [[io.taig.otter.operation.PrimitiveOperation.Text]], so it binds to any format
  * whose text primitive is a [[io.taig.otter.Wrapper.Primitive.Text]]. Mix it in next to a format's own vocabulary,
  * where the instance is found in the schema's companion:
  *
  * ```scala
  * object json extends JsonComponent, JavaTimeComponent[Json.Primitive.Text.Schema]
  *
  * json.field("published", json.instant)
  * ```
  *
  * Names follow JSON Schema's `format` where one is registered, so `date-time` is shared by [[instant]] and
  * [[offsetDateTime]], and `duration` by [[duration]] and [[period]]: each pair is the same text. A name is only what a
  * failure is reported against, so sharing one costs nothing.
  */
trait JavaTimeComponent[F[-_, +_]](using F: PrimitiveOperation.Text[F]):
  /** Every member is a round trip through `toString`, so only the way back has to be given; where `toString` writes
    * something the format's name does not promise, the way out is given too.
    *
    * That is every member carrying a time of day: `toString` leaves the seconds off a whole minute and writes `12:00`,
    * which RFC 3339 has no reading of -- its `time-second` is not optional. The `ISO_*` formatters always spell the
    * seconds out. Nothing changes on the way in, where both spellings are read.
    *
    * `DateTimeException` rather than `DateTimeParseException`, because `ZoneId.of` and `ZoneOffset.of` throw the
    * former. It is the supertype of both, so one catch covers parsing and zone lookup alike.
    */
  private def temporal[A](
      name: String,
      parse: String => A,
      print: A => String = (value: A) => value.toString
  ): F[A, A] = F.format(
    name,
    value => Either.catchOnly[DateTimeException](parse(value)).leftMap(_.getMessage),
    print
  )

  val instant: F[Instant, Instant] = temporal("date-time", Instant.parse)

  val offsetDateTime: F[OffsetDateTime, OffsetDateTime] =
    temporal("date-time", OffsetDateTime.parse, DateTimeFormatter.ISO_OFFSET_DATE_TIME.format)

  /** Not `date-time`: the text ends in a bracketed region, which RFC 3339 has no room for. */
  val zonedDateTime: F[ZonedDateTime, ZonedDateTime] =
    temporal("zoned-date-time", ZonedDateTime.parse, DateTimeFormatter.ISO_ZONED_DATE_TIME.format)

  val localDateTime: F[LocalDateTime, LocalDateTime] =
    temporal("local-date-time", LocalDateTime.parse, DateTimeFormatter.ISO_LOCAL_DATE_TIME.format)

  val localDate: F[LocalDate, LocalDate] = temporal("date", LocalDate.parse)

  /** Not `time`: that is RFC 3339 full time, which carries an offset. [[offsetTime]] is the one that does. */
  val localTime: F[LocalTime, LocalTime] =
    temporal("local-time", LocalTime.parse, DateTimeFormatter.ISO_LOCAL_TIME.format)

  val offsetTime: F[OffsetTime, OffsetTime] =
    temporal("time", OffsetTime.parse, DateTimeFormatter.ISO_OFFSET_TIME.format)

  val duration: F[Duration, Duration] = temporal("duration", Duration.parse)

  val period: F[Period, Period] = temporal("duration", Period.parse)

  val year: F[Year, Year] = temporal("year", Year.parse)

  val yearMonth: F[YearMonth, YearMonth] = temporal("year-month", YearMonth.parse)

  val monthDay: F[MonthDay, MonthDay] = temporal("month-day", MonthDay.parse)

  val zoneId: F[ZoneId, ZoneId] = temporal("zone-id", ZoneId.of)

  val zoneOffset: F[ZoneOffset, ZoneOffset] = temporal("zone-offset", ZoneOffset.of)
