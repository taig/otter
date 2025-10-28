package io.taig.otter.component

import io.taig.otter.operation.StringOperation
import java.time.Instant
import cats.syntax.all.*
import java.time.format.DateTimeParseException
import java.time.Duration
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.time.Period
import java.time.Year
import java.time.YearMonth
import java.time.ZonedDateTime
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.MonthDay

trait JavaTimeComponent[+Self[_]](using operation: StringOperation[Self]):
  private def parse[A](name: String, decode: String => A): Self[A] = operation.parser(
    name,
    decode = value => Either.catchOnly[DateTimeParseException](decode(value)).leftMap(_.getMessage),
    encode = _.toString
  )

  val duration: Self[Duration] = parse(name = "iso8601[duration]", decode = Duration.parse)
  val instant: Self[Instant] = parse(name = "iso8601[instant]", decode = Instant.parse)
  val localDate: Self[LocalDate] = parse(name = "iso8601[localDate]", decode = LocalDate.parse)
  val localDateTime: Self[LocalDateTime] = parse(name = "iso8601[localDateTime]", decode = LocalDateTime.parse)
  val localTime: Self[LocalTime] = parse(name = "iso8601[localTime]", decode = LocalTime.parse)
  val monthDay: Self[MonthDay] = parse(name = "iso8601[monthDay]", decode = MonthDay.parse)
  val offsetDateTime: Self[OffsetDateTime] = parse(name = "iso8601[offsetDateTime]", decode = OffsetDateTime.parse)
  val offsetTime: Self[OffsetTime] = parse(name = "iso8601[offsetTime]", decode = OffsetTime.parse)
  val period: Self[Period] = parse(name = "iso8601[period]", decode = Period.parse)
  val year: Self[Year] = parse(name = "iso8601[year]", decode = Year.parse)
  val yearMonth: Self[YearMonth] = parse(name = "iso8601[yearMonth]", decode = YearMonth.parse)
  val zonedDateTime: Self[ZonedDateTime] = parse(name = "iso8601[zonedDateTime]", decode = ZonedDateTime.parse)
  val zoneId: Self[ZoneId] = parse(name = "zoneId", decode = ZoneId.of)
  val zoneOffset: Self[ZoneOffset] = parse(name = "iso8601[zoneOffset]", decode = ZoneOffset.of)
