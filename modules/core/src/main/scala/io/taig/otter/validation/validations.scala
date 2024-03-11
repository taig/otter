package io.taig.otter.validation

import cats.UnorderedFoldable
import cats.data.{NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.otter.Data
import org.typelevel.ci.CIString

import java.time.format.DateTimeParseException
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID
import java.util.regex.Pattern
import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

trait validations:
  def equal(reference: String): Validation[String, Unit] =
    Validation.of(Constraint.Equals(reference)): value =>
      Validated.condNec(value == reference, (), Data.String(value))

  def equal(reference: CIString): Validation[CIString, Unit] =
    Validation.of(Constraint.Equals(reference.toString)): value =>
      Validated.condNec(value == reference, (), Data.String(value.toString))

  def exactLength[A](reference: Int, toLength: A => Int): Validation[A, Unit] =
    minLength(reference, toLength) *> maxLength(reference, toLength)

  def exactLength(reference: Int): Validation[String, Unit] = exactLength(reference, _.length)

  val email: Validation[CIString, Unit] = Validation
    .lift[CIString, String](_.toString)
    .andThen(matches(Pattern.compile(".+@.+")))

  def minimum[A: Numeric](
      reference: A,
      exclusive: Boolean,
      toData: A => Data.Number
  ): Validation[A, Unit] = Validation.of(Constraint.Minimum(toData(reference), exclusive)): value =>
    Validated.condNec(if exclusive then value > reference else value >= reference, (), toData(value))

  def minimum(reference: Int, exclusive: Boolean): Validation[Int, Unit] =
    minimum(reference, exclusive, Data.Number.apply)
  def minimum(reference: Int): Validation[Int, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: Long, exclusive: Boolean): Validation[Long, Unit] =
    minimum(reference, exclusive, Data.Number.apply)
  def minimum(reference: Long): Validation[Long, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: Float, exclusive: Boolean): Validation[Float, Unit] =
    minimum(reference, exclusive, Data.Number.apply)
  def minimum(reference: Float): Validation[Float, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: Double, exclusive: Boolean): Validation[Double, Unit] =
    minimum(reference, exclusive, Data.Number.apply)
  def minimum(reference: Double): Validation[Double, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: BigDecimal, exclusive: Boolean): Validation[BigDecimal, Unit] =
    minimum(reference, exclusive, Data.Number.apply)
  def minimum(reference: BigDecimal): Validation[BigDecimal, Unit] = minimum(reference, exclusive = false)

  def minimum(reference: BigInt, exclusive: Boolean): Validation[BigInt, Unit] =
    minimum(reference, exclusive, Data.Number.apply)
  def minimum(reference: BigInt): Validation[BigInt, Unit] = minimum(reference, exclusive = false)

  def maximum[A: Numeric](
      reference: A,
      exclusive: Boolean,
      toData: A => Data.Number
  ): Validation[A, Unit] = Validation.of(Constraint.Maximum(toData(reference), exclusive)): value =>
    Validated.condNec(if exclusive then value < reference else value <= reference, (), toData(value))

  def maximum(reference: Int, exclusive: Boolean): Validation[Int, Unit] =
    maximum(reference, exclusive, Data.Number.apply)
  def maximum(reference: Int): Validation[Int, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: Long, exclusive: Boolean): Validation[Long, Unit] =
    maximum(reference, exclusive, Data.Number.apply)
  def maximum(reference: Long): Validation[Long, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: Float, exclusive: Boolean): Validation[Float, Unit] =
    maximum(reference, exclusive, Data.Number.apply)
  def maximum(reference: Float): Validation[Float, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: Double, exclusive: Boolean): Validation[Double, Unit] =
    maximum(reference, exclusive, Data.Number.apply)
  def maximum(reference: Double): Validation[Double, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: BigDecimal, exclusive: Boolean): Validation[BigDecimal, Unit] =
    maximum(reference, exclusive, Data.Number.apply)
  def maximum(reference: BigDecimal): Validation[BigDecimal, Unit] = maximum(reference, exclusive = false)

  def maximum(reference: BigInt, exclusive: Boolean): Validation[BigInt, Unit] =
    maximum(reference, exclusive, Data.Number.apply)
  def maximum(reference: BigInt): Validation[BigInt, Unit] = maximum(reference, exclusive = false)

  def multiple[A: Integral](reference: A, toData: A => Data.Number): Validation[A, Unit] =
    Validation.of(Constraint.Multiple(toData(reference))): value =>
      Validated.condNec(value % reference == 0, (), toData(value))

  def multiple(reference: Int): Validation[Int, Unit] = multiple(reference, Data.Number.apply)

  def minLength[A](reference: Int, toLength: A => Int): Validation[A, Unit] =
    Validation.of(Constraint.MinLength(reference)): value =>
      val length = toLength(value)
      Validated.condNec(length >= reference, (), Data.Number(length))

  def minLength(reference: Int): Validation[String, Unit] = minLength(reference, _.length)

  def maxLength[A](reference: Int, toLength: A => Int): Validation[A, Unit] =
    Validation.of(Constraint.MaxLength(reference)): value =>
      val length = toLength(value)
      Validated.condNec(length <= reference, (), Data.Number(length))

  def maxLength(reference: Int): Validation[String, Unit] = maxLength(reference, _.length)

  def length[A](reference: Int, toLength: A => Int): Validation[A, Unit] =
    minLength(reference, toLength) *> maxLength(reference, toLength)

  def length(reference: Int): Validation[String, Unit] = length(reference, _.length)

  def matches(pattern: Pattern): Validation[String, Unit] = Validation.of(Constraint.Matches(pattern)): value =>
    Validated.condNec(pattern.matcher(value).matches(), (), Data.String(value))

  def minItems[A](reference: Long, count: A => Long): Validation[A, Unit] =
    Validation.of(Constraint.MinItems(reference)): values =>
      val size = count(values)
      Validated.condNec(size >= reference, (), Data.Number(size))

  def minItems[F[_]: UnorderedFoldable, A](reference: Long): Validation[F[A], Unit] = minItems(reference, _.size)

  def maxItems[A](reference: Long, count: A => Long): Validation[A, Unit] =
    Validation.of(Constraint.MaxItems(reference)): values =>
      val size = count(values)
      Validated.condNec(size <= reference, (), Data.Number(size))

  def maxItems[F[_]: UnorderedFoldable, A](reference: Long): Validation[F[A], Unit] = maxItems(reference, _.size)

  def parse[A](tpe: String)(f: String => Option[A]): Validation[String, A] =
    Validation.of(Constraint.Type(tpe)): value =>
      Validated.fromOption(f(value), NonEmptyChain.one(Data.String(value)))

  val required: Validation[String, String] = Validation.lift[String, String](_.trim).andThen(minLength(1).tap)

  val uuid: Validation[String, UUID] = parse("uuid"): value =>
    try UUID.fromString(value).some
    catch case _: IllegalArgumentException => none

  val date: Validation[String, LocalDate] = parse("date"): value =>
    try LocalDate.parse(value).some
    catch case _: DateTimeParseException => none

  val dateTime: Validation[String, LocalDateTime] = parse("date-time"): value =>
    try LocalDateTime.parse(value).some
    catch case _: DateTimeParseException => none

object validations extends validations
