package io.taig.otter.validation

import java.util.regex.Pattern
import cats.data.Validated
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.util.chaining.*
import scala.collection.immutable.Iterable

trait Validations:
  def matches(pattern: Pattern): Validation[String, Pattern, String, Unit] =
    Validation.when(pattern)(pattern.matcher(_).matches())

  def maxItems[A](reference: Long, count: A => Long): Validation[A, reference.type, Long, Unit] =
    Validation.validated(reference): a =>
      val size = count(a)
      Validated.condNec(size >= reference, (), size.toLong)

  def minItems[A](reference: Long, count: A => Long): Validation[A, reference.type, Long, Unit] =
    Validation.validated(reference): a =>
      val size = count(a)
      Validated.condNec(size >= reference, (), size.toLong)

  def maxProperties[A](reference: Long, count: A => Long): Validation[A, reference.type, Long, Unit] =
    Validation.validated(reference): a =>
      val size = count(a)
      Validated.condNec(size <= reference, (), size)

  def minProperties[A](reference: Long, count: A => Long): Validation[A, reference.type, Long, Unit] =
    Validation.validated(reference): a =>
      val size = count(a)
      Validated.condNec(size >= reference, (), size)

  def nonEmpty[A, B](uncons: A => Option[(B, A)]): Validation[A, 1L, 0L, (B, A)] =
    Validation.validated(1L)(fa => uncons(fa).toValidNec[0L](0L))

  def uniqueItems[F[a] <: Iterable[a], A]: Validation[F[A], "uniqueItems", NonEmptyList[A], Unit] =
    Validation.validated("uniqueItems"):
      _.groupBy(identity)
        .collect { case (a, as) if as.sizeCompare(1) > 0 => a }
        .toList
        .pipe(NonEmptyList.fromList)
        .toInvalidNec(())

// def equal(reference: String): Validation[String, Nothing, String, Unit] =
//   matches(Pattern.compile(Pattern.quote(reference)))

// def minimum[A: Numeric](reference: A, exclusive: Boolean): Validation[A, A, A, Unit] =
//   Validation.validated(Constraint.Minimum(reference, exclusive)): a =>
//     Validated.condNec(if exclusive then a > reference else a >= reference, (), a)

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

// def minLength[A](reference: Int, toLength: A => Int): Validation[A, Nothing, Int, Unit] =
//   Validation.validated(Constraint.MinLength(reference)): a =>
//     val length = toLength(a)
//     Validated.condNec(length >= reference, (), length)

// def minLength(reference: Int): Validation[CharSequence, Nothing, Int, Unit] = minLength(reference, _.length)

// def maxLength[A](reference: Int, toLength: A => Int): Validation[A, Nothing, Int, Unit] =
//   Validation.validated(Constraint.MaxLength(reference)): a =>
//     val length = toLength(a)
//     Validated.condNec(length <= reference, (), length)

// def maxLength(reference: Int): Validation[String, Nothing, Int, Unit] = maxLength(reference, _.length)

// def length[A](reference: Int, toLength: A => Int): Validation[A, Nothing, Int, Unit] =
//   minLength(reference, toLength) *> maxLength(reference, toLength)

// def length(reference: Int): Validation[String, Nothing, Int, Unit] = length(reference, _.length)

//   def minItems[A](reference: Long, count: A => Long): Validation[A, Unit] =
//     Validation.of(Constraint.MinItems(reference)): values =>
//       val size = count(values)
//       Validated.condNec(size >= reference, (), Data.Number(size))

//   def minItems[F[_]: UnorderedFoldable, A](reference: Long): Validation[F[A], Unit] = minItems(reference, _.size)

// def maxItems[A](reference: Long, count: A => Long): Validation[A, Long, Unit] =
//   Validation.cond(Constraint.MaxItems(reference))(a => count(a) <= reference)

// def maxItems[F[_]: UnorderedFoldable, A](reference: Long): Validation[F[A], Long, Unit] = maxItems(reference, _.size)

// def maxItems[F[a] <: IterableOnce[a], A](reference: Long): Validation[F[A], Long, Unit] =
//   maxItems(reference, _.iterator.size)

// def parse[A](tpe: String)(f: String => Option[A]): Validation[String, String, A] = Validation(Constraint.Type(tpe))(f)

// val required: Validation[String, String] = Validation.lift[String, String](_.trim).andThen(minLength(1).tap)

// val uuid: Validation[String, String, UUID] = parse("uuid"): value =>
//   try UUID.fromString(value).some
//   catch case _: IllegalArgumentException => none

// val date: Validation[String, String, LocalDate] = parse("date"): value =>
//   try LocalDate.parse(value).some
//   catch case _: DateTimeParseException => none

// val dateTime: Validation[String, String, LocalDateTime] = parse("dateTime"): value =>
//   try LocalDateTime.parse(value).some
//   catch case _: DateTimeParseException => none

// def oneOf[F[_]: UnorderedFoldable, A: Eq](references: Chain[A]): Validation[A, A, Unit] =
//   Validation(Constraint.OneOf(references)): a =>
//     Validated.cond(references.contains_(a), (), a)

// def uniqueItems[F[_]: Foldable, A: Order]: Validation[F[A], Unit] =
//   Validation.cond(Constraint.UniqueItems): fa =>
//     val (_, duplicate) = fa.foldl((SortedSet.empty[A], none[A])):
//       case ((set, None), a)                => (set, Option.when(set(a))(a))
//       case ((set, duplicate @ Some(_)), a) => (set, duplicate)

//     duplicate.isEmpty

object Validations extends Validations
