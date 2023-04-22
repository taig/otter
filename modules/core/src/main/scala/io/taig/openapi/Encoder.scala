package io.taig.openapi

import cats.data.{Chain, NonEmptyList}
import cats.syntax.all.*
import cats.{Foldable, Traverse}
import io.taig.openapi.syntax.*

import java.time.*
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.*
import java.time.temporal.TemporalAccessor
import java.util.{Currency, UUID}

trait Encoder[-A]:
  def encode(a: A): OpenApi

  final def contramap[B](f: B => A): Encoder[B] = Encoder.instance(f andThen encode)

object Encoder extends Encoder1:
  trait Array[-A] extends Encoder[A]:
    override def encode(a: A): OpenApi.Array[OpenApi]

  trait Object[-A] extends Encoder[A]:
    override def encode(a: A): OpenApi.Object

  given openapi: Encoder[OpenApi] = instance(identity)

  given bigDecimal: Encoder[BigDecimal] = instance(OpenApi.fromBigDecimal)

  given bigInt: Encoder[BigInt] = instance(OpenApi.fromBigInt)

  given boolean: Encoder[Boolean] = instance(OpenApi.fromBoolean)

  given byte: Encoder[Byte] = instance(OpenApi.fromByte)

  given double: Encoder[Double] = instance(OpenApi.fromDouble)

  given float: Encoder[Float] = instance(OpenApi.fromFloat)

  given int: Encoder[Int] = instance(OpenApi.fromInt)

  given long: Encoder[Long] = instance(OpenApi.fromLong)

  given short: Encoder[Short] = instance(OpenApi.fromShort)

  given string: Encoder[String] = instance(OpenApi.fromString)

  given duration: Encoder[Duration] = string.contramap(_.toString)

  given instant: Encoder[Instant] = string.contramap(_.toString)

  given period: Encoder[Period] = string.contramap(_.toString)

  given zoneId: Encoder[ZoneId] = string.contramap(_.toString)

  given uuid: Encoder[UUID] = string.contramap(_.toString)

  given unit: Encoder[Unit] = instance(_ => OpenApi.Null)

  given nothing: Encoder[Nothing] = instance(_ => OpenApi.Null)

  given option[A: Encoder]: Encoder[Option[A]] = instance {
    case Some(a) => a.asOpenApi
    case None    => OpenApi.Null
  }

  given some[A: Encoder]: Encoder[Some[A]] = instance(_.asOpenApi)

  given none: Encoder[None.type] = instance(_ => OpenApi.Null)

  given iterable[F[a] <: Iterable[a], A: Encoder]: Encoder[F[A]] =
    instance(values => OpenApi.fromVector(values.map(_.asOpenApi).toVector))

  given map[A: Encoder]: Encoder[Map[String, A]] = instance(values => OpenApi.Object(values.fmap(_.asOpenApi)))

  def temporalAccessor[A <: TemporalAccessor](formatter: DateTimeFormatter): Encoder[A] =
    string.contramap(formatter.format)

  def localDate(formatter: DateTimeFormatter): Encoder[LocalDate] = temporalAccessor(formatter)

  def localTime(formatter: DateTimeFormatter): Encoder[LocalTime] = temporalAccessor(formatter)

  def localDateTime(formatter: DateTimeFormatter): Encoder[LocalDateTime] = temporalAccessor(formatter)

  def monthDay(formatter: DateTimeFormatter): Encoder[MonthDay] = temporalAccessor(formatter)

  def offsetTime(formatter: DateTimeFormatter): Encoder[OffsetTime] = temporalAccessor(formatter)

  def offsetDateTime(formatter: DateTimeFormatter): Encoder[OffsetDateTime] = temporalAccessor(formatter)

  def year(formatter: DateTimeFormatter): Encoder[Year] = temporalAccessor(formatter)

  def yearMonth(formatter: DateTimeFormatter): Encoder[YearMonth] = temporalAccessor(formatter)

  def zonedDateTime(formatter: DateTimeFormatter): Encoder[ZonedDateTime] = temporalAccessor(formatter)

  def zoneOffset(formatter: DateTimeFormatter): Encoder[ZoneOffset] = temporalAccessor(formatter)

  given localDate: Encoder[LocalDate] = string.contramap(_.toString)

  given localTime: Encoder[LocalTime] = localTime(ISO_LOCAL_TIME)

  given localDateTime: Encoder[LocalDateTime] = localDateTime(ISO_LOCAL_DATE_TIME)

  given monthDay: Encoder[MonthDay] = string.contramap(_.toString)

  given offsetTime: Encoder[OffsetTime] = offsetTime(ISO_OFFSET_TIME)

  given offsetDateTime: Encoder[OffsetDateTime] = offsetDateTime(ISO_OFFSET_DATE_TIME)

  given year: Encoder[Year] = string.contramap(_.toString)

  given yearMonth: Encoder[YearMonth] = string.contramap(_.toString)

  given zonedDateTime: Encoder[ZonedDateTime] = zonedDateTime(ISO_ZONED_DATE_TIME)

  given zoneOffset: Encoder[ZoneOffset] = string.contramap(_.toString)

  given currencyEncoder: Encoder[Currency] = string.contramap(_.getCurrencyCode())

  given emptyTuple: Encoder[EmptyTuple] = instance(_ => OpenApi.Array.Empty)

  given tuple1[A: Encoder]: Encoder[A *: EmptyTuple] = instance(a => OpenApi.Array.one(a.asOpenApi))

  given tuple2[A: Encoder, B: Encoder]: Encoder[(A, B)] =
    instance { case (a, b) => OpenApi.arr(a.asOpenApi, b.asOpenApi) }

  given tuple3[A: Encoder, B: Encoder, C: Encoder]: Encoder[(A, B, C)] =
    instance { case (a, b, c) => OpenApi.arr(a.asOpenApi, b.asOpenApi, c.asOpenApi) }

trait Encoder1:
  final inline def apply[A](implicit encoder: Encoder[A]): Encoder[A] = encoder

  final inline def instance[A](f: A => OpenApi): Encoder[A] = f(_)

  final given foldable[F[_]: Foldable, A: Encoder]: Encoder[F[A]] =
    instance(values => OpenApi.fromList(values.toList.map(_.asOpenApi)))
