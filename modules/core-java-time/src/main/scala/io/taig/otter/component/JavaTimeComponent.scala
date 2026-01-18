package io.taig.otter.component

import cats.syntax.all.*

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
import java.time.format.DateTimeParseException
import io.taig.otter.operation.PrimitiveOperation

trait JavaTimeComponent[F[_]](using F: PrimitiveOperation.Text[F]):
  private def parse[A](name: String, decode: String => A): F[A] = F.codec(
    name,
    parse = value => Either.catchOnly[DateTimeParseException](decode(value)).leftMap(_.getMessage),
    print = _.toString
  )

  private def isoType(name: String): String = s"iso8601[$name]"

  final val duration: F[Duration] = parse(name = isoType("duration"), decode = Duration.parse)
  final val instant: F[Instant] = parse(name = isoType("instant"), decode = Instant.parse)
  final val localDate: F[LocalDate] = parse(name = isoType("localDate"), decode = LocalDate.parse)
  final val localDateTime: F[LocalDateTime] = parse(name = isoType("localDateTime"), decode = LocalDateTime.parse)
  final val localTime: F[LocalTime] = parse(name = isoType("localTime"), decode = LocalTime.parse)
  final val monthDay: F[MonthDay] = parse(name = isoType("monthDay"), decode = MonthDay.parse)
  final val offsetDateTime: F[OffsetDateTime] = parse(name = isoType("offsetDateTime"), decode = OffsetDateTime.parse)
  final val offsetTime: F[OffsetTime] = parse(name = isoType("offsetTime"), decode = OffsetTime.parse)
  final val period: F[Period] = parse(name = isoType("period"), decode = Period.parse)
  final val year: F[Year] = parse(name = isoType("year"), decode = Year.parse)
  final val yearMonth: F[YearMonth] = parse(name = isoType("yearMonth"), decode = YearMonth.parse)
  final val zonedDateTime: F[ZonedDateTime] = parse(name = isoType("zonedDateTime"), decode = ZonedDateTime.parse)
  final val zoneId: F[ZoneId] = parse(name = "zoneId", decode = ZoneId.of)
  final val zoneOffset: F[ZoneOffset] = parse(name = isoType("zoneOffset"), decode = ZoneOffset.of)
