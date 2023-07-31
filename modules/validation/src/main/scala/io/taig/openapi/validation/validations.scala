package io.taig.openapi.validation

import cats.data.Validated
import cats.syntax.all.*

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeParseException
import java.util.UUID

object validations:
  private def minimum[A](
      reference: A,
      exclusive: Boolean,
      gt: (A, A) => Boolean,
      gteq: (A, A) => Boolean,
      toBigDecimal: A => BigDecimal
  ): Validation[A, A, Unit] = Validation(Constraint.Minimum(toBigDecimal(reference), exclusive)): value =>
    Validated.condNec(if exclusive then gt(value, reference) else gteq(value, reference), (), value.some)

  def minimum(reference: Int, exclusive: Boolean): Validation[Int, Int, Unit] =
    minimum(reference, exclusive, _ > _, _ >= _, BigDecimal.apply)
  def minimum(reference: Int): Validation[Int, Int, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: Long, exclusive: Boolean): Validation[Long, Long, Unit] =
    minimum(reference, exclusive, _ > _, _ >= _, BigDecimal.apply)
  def minimum(reference: Long): Validation[Long, Long, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: Float, exclusive: Boolean): Validation[Float, Float, Unit] =
    minimum(reference, exclusive, _ > _, _ >= _, BigDecimal.apply)
  def minimum(reference: Float): Validation[Float, Float, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: Double, exclusive: Boolean): Validation[Double, Double, Unit] =
    minimum(reference, exclusive, _ > _, _ >= _, BigDecimal.apply)
  def minimum(reference: Double): Validation[Double, Double, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: BigDecimal, exclusive: Boolean): Validation[BigDecimal, BigDecimal, Unit] =
    minimum(reference, exclusive, _ > _, _ >= _, identity)
  def minimum(reference: BigDecimal): Validation[BigDecimal, BigDecimal, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: BigInt, exclusive: Boolean): Validation[BigInt, BigInt, Unit] =
    minimum(reference, exclusive, _ > _, _ >= _, BigDecimal.apply)
  def minimum(reference: BigInt): Validation[BigInt, BigInt, Unit] = minimum(reference, exclusive = false)
//
//  private def maximum[A](
//      reference: A,
//      exclusive: Boolean,
//      lt: (A, A) => Boolean,
//      lteq: (A, A) => Boolean,
//      toNumber: A => OpenApi.Number
//  ): Validation[A, Unit] = Validation(Constraint.Maximum(toNumber(reference), exclusive)): value =>
//    Validated.condNec(if exclusive then lt(value, reference) else lteq(value, reference), (), toNumber(value).some)
//
//  def maximum(reference: Int): Validation[Int, Unit] =
//    maximum(reference, exclusive = false, _ < _, _ <= _, OpenApi.Int.apply)
//
//  def maximum(reference: Int, exclusive: Boolean): Validation[Int, Unit] =
//    maximum(reference, exclusive, _ < _, _ <= _, OpenApi.Int.apply)
//
//  def maximum(reference: Long): Validation[Long, Unit] =
//    maximum(reference, exclusive = false, _ < _, _ <= _, OpenApi.Long.apply)
//
//  def maximum(reference: Long, exclusive: Boolean): Validation[Long, Unit] =
//    maximum(reference, exclusive, _ < _, _ <= _, OpenApi.Long.apply)
//
//  def maximum(reference: Float): Validation[Float, Unit] =
//    maximum(reference, exclusive = false, _ < _, _ <= _, OpenApi.Float.apply)
//
//  def maximum(reference: Float, exclusive: Boolean): Validation[Float, Unit] =
//    maximum(reference, exclusive, _ < _, _ <= _, OpenApi.Float.apply)
//
//  def maximum(reference: Double): Validation[Double, Unit] =
//    maximum(reference, exclusive = false, _ < _, _ <= _, OpenApi.Double.apply)
//
//  def maximum(reference: Double, exclusive: Boolean): Validation[Double, Unit] =
//    maximum(reference, exclusive, _ < _, _ <= _, OpenApi.Double.apply)
//
//  def maximum(reference: BigDecimal): Validation[BigDecimal, Unit] =
//    maximum(reference, exclusive = false, _ < _, _ <= _, OpenApi.BigDecimal.apply)
//
//  def maximum(reference: BigDecimal, exclusive: Boolean): Validation[BigDecimal, Unit] =
//    maximum(reference, exclusive, _ < _, _ <= _, OpenApi.BigDecimal.apply)
//
//  def maximum(reference: BigInt): Validation[BigInt, Unit] =
//    maximum(reference, exclusive = false, _ < _, _ <= _, OpenApi.BigInt.apply)
//
//  def maximum(reference: BigInt, exclusive: Boolean): Validation[BigInt, Unit] =
//    maximum(reference, exclusive, _ < _, _ <= _, OpenApi.BigInt.apply)
//
//  def minLength(reference: Int): Validation[String, Unit] = Validation(Constraint.MinLength(reference)): value =>
//    Validated.condNec(
//      value.length >= reference,
//      (),
//      OpenApi.fromInt(value.length).some
//    )
//
//  def maxLength(reference: Int): Validation[String, Unit] = Validation(Constraint.MaxLength(reference)): value =>
//    Validated.condNec(
//      value.length <= reference,
//      (),
//      OpenApi.fromInt(value.length).some
//    )
//
//  val uuid: Validation[String, UUID] = Validation.parse("uuid"): value =>
//    try UUID.fromString(value).some
//    catch case _: IllegalArgumentException => none
//
//  val date: Validation[String, LocalDate] = Validation.parse("date"): value =>
//    try LocalDate.parse(value).some
//    catch case _: DateTimeParseException => none
//
//  val dateTime: Validation[String, LocalDateTime] = Validation.parse("date-time"): value =>
//    try LocalDateTime.parse(value).some
//    catch case _: DateTimeParseException => none
