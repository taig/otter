package io.taig.otter.validation

import cats.Eq
import cats.Foldable
import cats.Order
import cats.data.Chain
import cats.data.Validated
import cats.implicits.*

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.format.DateTimeParseException
import java.util.UUID
import java.util.regex.Pattern
import scala.collection.immutable.SortedSet
import cats.UnorderedFoldable

trait validations:
  def equal[A: Eq](reference: A): Validation[A, A, Unit] = Validation(Constraint.Equals(reference)): value =>
    Validated.cond(value === reference, (), value)

//   def exactLength[A](reference: Int, toLength: A => Int): Validation[A, Unit] =
//     minLength(reference, toLength) *> maxLength(reference, toLength)

//   def exactLength(reference: Int): Validation[String, Unit] = exactLength(reference, _.length)

//   val email: Validation[CIString, Unit] = Validation
//     .lift[CIString, String](_.toString)
//     .andThen(matches(Pattern.compile(".+@.+")))

//   def minimum[A: Numeric](
//       reference: A,
//       exclusive: Boolean,
//       toData: A => Data.Number
//   ): Validation[A, Unit] = Validation.of(Constraint.Minimum(toData(reference), exclusive)): value =>
//     Validated.condNec(if exclusive then value > reference else value >= reference, (), toData(value))

//   def minimum(reference: Int, exclusive: Boolean): Validation[Int, Unit] =
//     minimum(reference, exclusive, Data.Number.apply)
//   def minimum(reference: Int): Validation[Int, Unit] = minimum(reference, exclusive = false)

//   def minimum(reference: Long, exclusive: Boolean): Validation[Long, Unit] =
//     minimum(reference, exclusive, Data.Number.apply)
//   def minimum(reference: Long): Validation[Long, Unit] = minimum(reference, exclusive = false)

//   def minimum(reference: Float, exclusive: Boolean): Validation[Float, Unit] =
//     minimum(reference, exclusive, Data.Number.apply)
//   def minimum(reference: Float): Validation[Float, Unit] = minimum(reference, exclusive = false)

//   def minimum(reference: Double, exclusive: Boolean): Validation[Double, Unit] =
//     minimum(reference, exclusive, Data.Number.apply)
//   def minimum(reference: Double): Validation[Double, Unit] = minimum(reference, exclusive = false)

//   def minimum(reference: BigDecimal, exclusive: Boolean): Validation[BigDecimal, Unit] =
//     minimum(reference, exclusive, Data.Number.apply)
//   def minimum(reference: BigDecimal): Validation[BigDecimal, Unit] = minimum(reference, exclusive = false)

//   def minimum(reference: BigInt, exclusive: Boolean): Validation[BigInt, Unit] =
//     minimum(reference, exclusive, Data.Number.apply)
//   def minimum(reference: BigInt): Validation[BigInt, Unit] = minimum(reference, exclusive = false)

//   def maximum[A: Numeric](
//       reference: A,
//       exclusive: Boolean,
//       toData: A => Data.Number
//   ): Validation[A, Unit] = Validation.of(Constraint.Maximum(toData(reference), exclusive)): value =>
//     Validated.condNec(if exclusive then value < reference else value <= reference, (), toData(value))

//   def maximum(reference: Int, exclusive: Boolean): Validation[Int, Unit] =
//     maximum(reference, exclusive, Data.Number.apply)
//   def maximum(reference: Int): Validation[Int, Unit] = maximum(reference, exclusive = false)

//   def maximum(reference: Long, exclusive: Boolean): Validation[Long, Unit] =
//     maximum(reference, exclusive, Data.Number.apply)
//   def maximum(reference: Long): Validation[Long, Unit] = maximum(reference, exclusive = false)

//   def maximum(reference: Float, exclusive: Boolean): Validation[Float, Unit] =
//     maximum(reference, exclusive, Data.Number.apply)
//   def maximum(reference: Float): Validation[Float, Unit] = maximum(reference, exclusive = false)

//   def maximum(reference: Double, exclusive: Boolean): Validation[Double, Unit] =
//     maximum(reference, exclusive, Data.Number.apply)
//   def maximum(reference: Double): Validation[Double, Unit] = maximum(reference, exclusive = false)

//   def maximum(reference: BigDecimal, exclusive: Boolean): Validation[BigDecimal, Unit] =
//     maximum(reference, exclusive, Data.Number.apply)
//   def maximum(reference: BigDecimal): Validation[BigDecimal, Unit] = maximum(reference, exclusive = false)

//   def maximum(reference: BigInt, exclusive: Boolean): Validation[BigInt, Unit] =
//     maximum(reference, exclusive, Data.Number.apply)
//   def maximum(reference: BigInt): Validation[BigInt, Unit] = maximum(reference, exclusive = false)

//   def multiple[A: Integral](reference: A, toData: A => Data.Number): Validation[A, Unit] =
//     Validation.of(Constraint.Multiple(toData(reference))): value =>
//       Validated.condNec(value % reference == 0, (), toData(value))

//   def multiple(reference: Int): Validation[Int, Unit] = multiple(reference, Data.Number.apply)

//   def minLength[A](reference: Int, toLength: A => Int): Validation[A, Unit] =
//     Validation.of(Constraint.MinLength(reference)): value =>
//       val length = toLength(value)
//       Validated.condNec(length >= reference, (), Data.Number(length))

//   def minLength(reference: Int): Validation[String, Unit] = minLength(reference, _.length)

//   def maxLength[A](reference: Int, toLength: A => Int): Validation[A, Unit] =
//     Validation.of(Constraint.MaxLength(reference)): value =>
//       val length = toLength(value)
//       Validated.condNec(length <= reference, (), Data.Number(length))

//   def maxLength(reference: Int): Validation[String, Unit] = maxLength(reference, _.length)

//   def length[A](reference: Int, toLength: A => Int): Validation[A, Unit] =
//     minLength(reference, toLength) *> maxLength(reference, toLength)

//   def length(reference: Int): Validation[String, Unit] = length(reference, _.length)

  def matches(pattern: Pattern): Validation[String, String, Unit] = Validation(Constraint.Matches(pattern)): value =>
    Validated.cond(pattern.matcher(value).matches(), (), value)

//   def minItems[A](reference: Long, count: A => Long): Validation[A, Unit] =
//     Validation.of(Constraint.MinItems(reference)): values =>
//       val size = count(values)
//       Validated.condNec(size >= reference, (), Data.Number(size))

//   def minItems[F[_]: UnorderedFoldable, A](reference: Long): Validation[F[A], Unit] = minItems(reference, _.size)

  def maxItems[A](reference: Long, count: A => Long): Validation[A, Long, Unit] =
    Validation(Constraint.MaxItems(reference)): a =>
      val size = count(a)
      Validated.cond(size <= reference, (), size)

  def maxItems[F[_]: UnorderedFoldable, A](reference: Long): Validation[F[A], Long, Unit] = maxItems(reference, _.size)

  def maxItems[F[a] <: IterableOnce[a], A](reference: Long): Validation[F[A], Long, Unit] =
    maxItems(reference, _.iterator.size)

  def parse[A](tpe: String)(f: String => Option[A]): Validation[String, String, A] =
    Validation(Constraint.Type(tpe))(value => f(value).toValid(value))

//   val required: Validation[String, String] = Validation.lift[String, String](_.trim).andThen(minLength(1).tap)

  val uuid: Validation[String, String, UUID] = parse("uuid"): value =>
    try UUID.fromString(value).some
    catch case _: IllegalArgumentException => none

  val date: Validation[String, String, LocalDate] = parse("date"): value =>
    try LocalDate.parse(value).some
    catch case _: DateTimeParseException => none

  val dateTime: Validation[String, String, LocalDateTime] = parse("dateTime"): value =>
    try LocalDateTime.parse(value).some
    catch case _: DateTimeParseException => none

  def oneOf[F[_]: UnorderedFoldable, A: Eq](references: Chain[A]): Validation[A, A, Unit] =
    Validation(Constraint.OneOf(references)): a =>
      Validated.cond(references.contains_(a), (), a)

  def uniqueItems[F[_]: Foldable, A: Order]: Validation[F[A], A, Unit] =
    Validation(Constraint.UniqueItems): fa =>
      val (_, duplicate) = fa.foldl((SortedSet.empty[A], none[A])):
        case ((set, None), a)                => (set, Option.when(set(a))(a))
        case ((set, duplicate @ Some(_)), a) => (set, duplicate)

      duplicate.toInvalid(())

object validations extends validations
