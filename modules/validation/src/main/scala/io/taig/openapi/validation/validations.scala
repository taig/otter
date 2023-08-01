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

  private def maximum[A](
      reference: A,
      exclusive: Boolean,
      lt: (A, A) => Boolean,
      lteq: (A, A) => Boolean,
      toBigDecimal: A => BigDecimal
  ): Validation[A, A, Unit] = Validation(Constraint.Maximum(toBigDecimal(reference), exclusive)): value =>
    Validated.condNec(if exclusive then lt(value, reference) else lteq(value, reference), (), value.some)

  def maximum(reference: Int, exclusive: Boolean): Validation[Int, Int, Unit] =
    maximum(reference, exclusive, _ < _, _ <= _, BigDecimal.apply)
  def maximum(reference: Int): Validation[Int, Int, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: Long, exclusive: Boolean): Validation[Long, Long, Unit] =
    maximum(reference, exclusive, _ < _, _ <= _, BigDecimal.apply)
  def maximum(reference: Long): Validation[Long, Long, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: Float, exclusive: Boolean): Validation[Float, Float, Unit] =
    maximum(reference, exclusive, _ < _, _ <= _, BigDecimal.apply)
  def maximum(reference: Float): Validation[Float, Float, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: Double, exclusive: Boolean): Validation[Double, Double, Unit] =
    maximum(reference, exclusive, _ < _, _ <= _, BigDecimal.apply)
  def maximum(reference: Double): Validation[Double, Double, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: BigDecimal, exclusive: Boolean): Validation[BigDecimal, BigDecimal, Unit] =
    maximum(reference, exclusive, _ < _, _ <= _, identity)
  def maximum(reference: BigDecimal): Validation[BigDecimal, BigDecimal, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: BigInt, exclusive: Boolean): Validation[BigInt, BigInt, Unit] =
    maximum(reference, exclusive, _ < _, _ <= _, BigDecimal.apply)
  def maximum(reference: BigInt): Validation[BigInt, BigInt, Unit] = maximum(reference, exclusive = false)

  def multiple(reference: Int): Validation[Int, Int, Unit] = Validation(Constraint.Multiple(reference)): value =>
    Validated.condNec(value % reference == 0, (), value.some)

  def minLength(reference: Int): Validation[Int, String, Unit] = Validation(Constraint.MinLength(reference)): value =>
    Validated.condNec(value.length >= reference, (), value.length.some)

  def maxLength(reference: Int): Validation[Int, String, Unit] = Validation(Constraint.MaxLength(reference)): value =>
    Validated.condNec(value.length <= reference, (), value.length.some)

  val uuid: Validation[String, String, UUID] = Validation.parse("uuid"): value =>
    try UUID.fromString(value).some
    catch case _: IllegalArgumentException => none

  val date: Validation[String, String, LocalDate] = Validation.parse("date"): value =>
    try LocalDate.parse(value).some
    catch case _: DateTimeParseException => none

  val dateTime: Validation[String, String, LocalDateTime] = Validation.parse("date-time"): value =>
    try LocalDateTime.parse(value).some
    catch case _: DateTimeParseException => none
