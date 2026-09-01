package io.taig.otter.component

import io.taig.otter.Constraint
import io.taig.otter.Primitive
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation
import zio.Scope
import zio.test.*

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

object JavaTimeComponentTest extends ZIOSpecDefault:
  /** The bare AST is a format too, and the only one `core` offers. */
  private given PrimitiveOperation.Text[Primitive.Text]:
    override def string(validation: Validation[Constraint.Primitive.Text, String]): Primitive.Text[String, String] =
      Primitive.Text.Root(validation)

    override def format[W, R](
        name: String,
        parse: String => Either[String, R],
        print: W => String
    ): Primitive.Text[W, R] = Primitive.Text.Format(name, parse, print)

  private object time extends JavaTimeComponent[Primitive.Text]

  /** The name it reports, the way in and the way back, which is all a format is. */
  private def roundTrip[A](schema: Primitive.Text[A, A], name: String, value: A, text: String): TestResult =
    schema match
      case Primitive.Text.Format(actual, parse, print) =>
        assertTrue(actual == name, parse(text) == Right(value), print(value) == text)
      case _ => assertTrue(false)

  private def reads[A](schema: Primitive.Text[A, A], text: String, value: A): TestResult = schema match
    case Primitive.Text.Format(_, parse, _) => assertTrue(parse(text) == Right(value))
    case _                                  => assertTrue(false)

  private def rejects[A](schema: Primitive.Text[A, A], text: String): TestResult = schema match
    case Primitive.Text.Format(_, parse, _) => assertTrue(parse(text).isLeft)
    case _                                  => assertTrue(false)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JavaTimeComponentTest")(
    test("instant"):
      roundTrip(time.instant, "date-time", Instant.parse("2026-08-31T12:00:00Z"), "2026-08-31T12:00:00Z")
    ,
    test("offsetDateTime"):
      val value = OffsetDateTime.of(2026, 8, 31, 12, 0, 0, 0, ZoneOffset.ofHours(2))
      roundTrip(time.offsetDateTime, "date-time", value, "2026-08-31T12:00:00+02:00")
    ,
    test("zonedDateTime"):
      val value = ZonedDateTime.of(2026, 8, 31, 12, 0, 0, 0, ZoneId.of("Europe/Berlin"))
      roundTrip(time.zonedDateTime, "zoned-date-time", value, "2026-08-31T12:00:00+02:00[Europe/Berlin]")
    ,
    test("localDateTime"):
      val value = LocalDateTime.of(2026, 8, 31, 12, 0)
      roundTrip(time.localDateTime, "local-date-time", value, "2026-08-31T12:00:00")
    ,
    test("localDate"):
      roundTrip(time.localDate, "date", LocalDate.of(2026, 8, 31), "2026-08-31")
    ,
    test("localTime"):
      roundTrip(time.localTime, "local-time", LocalTime.of(12, 0), "12:00:00")
    ,
    test("offsetTime"):
      val value = OffsetTime.of(12, 0, 0, 0, ZoneOffset.ofHours(2))
      roundTrip(time.offsetTime, "time", value, "12:00:00+02:00")
    ,
    test("duration"):
      roundTrip(time.duration, "duration", Duration.ofMinutes(510), "PT8H30M")
    ,
    test("period"):
      roundTrip(time.period, "duration", Period.of(1, 2, 3), "P1Y2M3D")
    ,
    test("year"):
      roundTrip(time.year, "year", Year.of(2026), "2026")
    ,
    test("yearMonth"):
      roundTrip(time.yearMonth, "year-month", YearMonth.of(2026, 8), "2026-08")
    ,
    test("monthDay"):
      roundTrip(time.monthDay, "month-day", MonthDay.of(8, 31), "--08-31")
    ,
    test("zoneId"):
      roundTrip(time.zoneId, "zone-id", ZoneId.of("Europe/Berlin"), "Europe/Berlin")
    ,
    test("zoneOffset"):
      roundTrip(time.zoneOffset, "zone-offset", ZoneOffset.ofHours(2), "+02:00")
    ,
    /** What `toString` writes, which is what a client that formats one of its own sends back. */
    test("text whose seconds are left out is read all the same"):
      val offset = ZoneOffset.ofHours(2)

      reads(time.offsetDateTime, "2026-08-31T12:00+02:00", OffsetDateTime.of(2026, 8, 31, 12, 0, 0, 0, offset)) &&
      reads(
        time.zonedDateTime,
        "2026-08-31T12:00+02:00[Europe/Berlin]",
        ZonedDateTime.of(2026, 8, 31, 12, 0, 0, 0, ZoneId.of("Europe/Berlin"))
      ) &&
      reads(time.localDateTime, "2026-08-31T12:00", LocalDateTime.of(2026, 8, 31, 12, 0)) &&
      reads(time.localTime, "12:00", LocalTime.of(12, 0)) &&
      reads(time.offsetTime, "12:00+02:00", OffsetTime.of(12, 0, 0, 0, offset))
    ,
    test("text that does not parse is a failure, not an exception"):
      rejects(time.instant, "nope") &&
      rejects(time.offsetDateTime, "nope") &&
      rejects(time.zonedDateTime, "nope") &&
      rejects(time.localDateTime, "nope") &&
      rejects(time.localDate, "2026-13-45") &&
      rejects(time.localTime, "nope") &&
      rejects(time.offsetTime, "nope") &&
      rejects(time.duration, "nope") &&
      rejects(time.period, "nope") &&
      rejects(time.year, "nope") &&
      rejects(time.yearMonth, "nope") &&
      rejects(time.monthDay, "nope")
    ,
    /** `ZoneId.of` and `ZoneOffset.of` throw `DateTimeException`, which a narrower catch would let escape. */
    test("a zone that does not resolve is a failure too"):
      rejects(time.zoneId, "Not/AZone") && rejects(time.zoneId, "nope") && rejects(time.zoneOffset, "nope")
  )
