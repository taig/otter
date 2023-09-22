package io.taig.otter.validation

import cats.UnorderedFoldable
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Schema
import io.taig.otter.schemas.*
import org.typelevel.ci.CIString
import scala.math.Ordering.Implicits.*

import java.time.format.DateTimeParseException
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

trait validations:
  def equal(reference: String): Validation[String, Unit] =
    Validation(Constraint.Equals(reference), string): value =>
      Validated.condNec(value == reference, (), value)

  def equal(reference: CIString): Validation[CIString, Unit] =
    Validation(Constraint.Equals(reference.toString), cistring): value =>
      Validated.condNec(value == reference, (), value)

  def minimum[A: Numeric](
      reference: A,
      exclusive: Boolean,
      schema: Schema[A]
  ): Validation[A, Unit] = Validation(Constraint.Minimum(reference, exclusive), schema): value =>
    Validated.condNec(if exclusive then value > reference else value >= reference, (), value)

  def minimum(reference: Int, exclusive: Boolean): Validation[Int, Unit] = minimum(reference, exclusive, int)
  def minimum(reference: Int): Validation[Int, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: Long, exclusive: Boolean): Validation[Long, Unit] = minimum(reference, exclusive, long)
  def minimum(reference: Long): Validation[Long, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: Float, exclusive: Boolean): Validation[Float, Unit] = minimum(reference, exclusive, float)
  def minimum(reference: Float): Validation[Float, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: Double, exclusive: Boolean): Validation[Double, Unit] =
    minimum(reference, exclusive, double)
  def minimum(reference: Double): Validation[Double, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: BigDecimal, exclusive: Boolean): Validation[BigDecimal, Unit] =
    minimum(reference, exclusive, bigDecimal)
  def minimum(reference: BigDecimal): Validation[BigDecimal, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: BigInt, exclusive: Boolean): Validation[BigInt, Unit] = minimum(reference, exclusive, bigInt)
  def minimum(reference: BigInt): Validation[BigInt, Unit] = minimum(reference, exclusive = false)

  def maximum[A: Numeric](
      reference: A,
      exclusive: Boolean,
      schema: Schema[A]
  ): Validation[A, Unit] = Validation(Constraint.Maximum(reference, exclusive), schema): value =>
    Validated.condNec(if exclusive then value < reference else value <= reference, (), value)

  def maximum(reference: Int, exclusive: Boolean): Validation[Int, Unit] = maximum(reference, exclusive, int)
  def maximum(reference: Int): Validation[Int, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: Long, exclusive: Boolean): Validation[Long, Unit] = maximum(reference, exclusive, long)
  def maximum(reference: Long): Validation[Long, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: Float, exclusive: Boolean): Validation[Float, Unit] = maximum(reference, exclusive, float)
  def maximum(reference: Float): Validation[Float, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: Double, exclusive: Boolean): Validation[Double, Unit] = maximum(reference, exclusive, double)
  def maximum(reference: Double): Validation[Double, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: BigDecimal, exclusive: Boolean): Validation[BigDecimal, Unit] =
    maximum(reference, exclusive, bigDecimal)
  def maximum(reference: BigDecimal): Validation[BigDecimal, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: BigInt, exclusive: Boolean): Validation[BigInt, Unit] = maximum(reference, exclusive, bigInt)
  def maximum(reference: BigInt): Validation[BigInt, Unit] = maximum(reference, exclusive = false)

  def multiple(reference: Int): Validation[Int, Unit] = Validation(Constraint.Multiple(reference), int): value =>
    Validated.condNec(value % reference == 0, (), value)

  def minLength(reference: Int): Validation[String, Unit] = Validation(Constraint.MinLength(reference), int): value =>
    Validated.condNec(value.length >= reference, (), value.length)

  def maxLength(reference: Int): Validation[String, Unit] = Validation(Constraint.MaxLength(reference), int): value =>
    Validated.condNec(value.length <= reference, (), value.length)

//  def minItems[A](reference: Long, count: A => Long): Validation[A, Unit] =
//    Validation(Constraint.MinItems(reference)): values =>
//      val size = count(values)
//      Validated.condNec(size >= reference, (), OpenApi.Integer(size))
//
//  def minItems[F[_]: UnorderedFoldable, A](reference: Long): Validation[F[A], Unit] = minItems(reference, _.size)
//
//  def maxItems[A](reference: Long, count: A => Long): Validation[A, Unit] =
//    Validation(Constraint.MaxItems(reference)): values =>
//      val size = count(values)
//      Validated.condNec(size <= reference, (), OpenApi.Integer(size))
//
//  def maxItems[F[_]: UnorderedFoldable, A](reference: Long): Validation[F[A], Unit] = maxItems(reference, _.size)
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

object validations extends validations
