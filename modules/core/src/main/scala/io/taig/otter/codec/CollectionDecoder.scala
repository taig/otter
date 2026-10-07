package io.taig.otter.codec

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Collection
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.validation.Comparison
import io.taig.validation.Validation
import io.taig.validation.Violation

import scala.collection.immutable.SortedSet

final class CollectionDecoder[F[-_, +_], T](decoder: Decoder[F, T]) extends Decoder[Collection[F, *, *], Seq[T]]:
  override def decode[R](schema: Collection[F, Nothing, R], values: Seq[T]): Validated[Violations, R] =
    schema match
      case Collection.Chained(reference, validation) =>
        elements(reference.value, values, validation)(Chain.fromSeq)
      case Collection.Indexed(reference, validation) => elements(reference.value, values, validation)(_.toVector)
      case Collection.Linked(reference, validation)  => elements(reference.value, values, validation)(_.toList)
      case Collection.NonEmptyChained(reference, validation) =>
        checkedElements(reference.value, values, validation)(values =>
          nonEmpty(values)((head, tail) => NonEmptyChain(head, tail*))
        )
      case Collection.NonEmptyIndexed(reference, validation) =>
        checkedElements(reference.value, values, validation)(values =>
          nonEmpty(values)((head, tail) => NonEmptyVector(head, tail.toVector))
        )
      case Collection.NonEmptyLinked(reference, validation) =>
        checkedElements(reference.value, values, validation)(values =>
          nonEmpty(values)((head, tail) => NonEmptyList(head, tail.toList))
        )
      case Collection.Sorted(reference, _, ordering, validation) =>
        checkedElements(reference.value, values, validation)(unique(_, ordering))
      case Collection.NonEmptySorted(reference, _, ordering, validation) =>
        checkedElements(reference.value, values, validation)(values =>
          unique(values, ordering).andThen(values => NonEmptySet.fromSet(values).toValid(CollectionDecoder.empty))
        )
      case Collection.Modify(self, f, _) => decode(self, values).map(f)

  private def elements[R, C](
      schema: F[Nothing, R],
      values: Seq[T],
      validation: Validation[Constraint.Collection, C]
  )(collect: Seq[R] => C): Validated[Violations, C] =
    values.zipWithIndex
      .traverse((value, index) => decoder.decode(schema, value).leftMap(index /: _))
      .map(collect)
      .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))

  private def checkedElements[R, C](
      schema: F[Nothing, R],
      values: Seq[T],
      validation: Validation[Constraint.Collection, C]
  )(collect: Seq[R] => Validated[Violations, C]): Validated[Violations, C] =
    values.zipWithIndex
      .traverse((value, index) => decoder.decode(schema, value).leftMap(index /: _))
      .andThen(collect)
      .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))

  private def nonEmpty[R, C](values: Seq[R])(collect: (R, Seq[R]) => C): Validated[Violations, C] =
    values.headOption.fold(CollectionDecoder.empty.invalid[C])(head => collect(head, values.tail).valid)

  /** Keep duplicate positions: a domain value need not have a data encoder, and the wire index remains meaningful even
    * when its ordering equates values that are otherwise distinct.
    */
  private def unique[R](values: Seq[R], ordering: Ordering[R]): Validated[Violations, SortedSet[R]] =
    val (collected, duplicates) = values.zipWithIndex.foldLeft((SortedSet.empty[R](using ordering), List.empty[Int])):
      case ((seen, duplicates), (value, index)) =>
        if seen.contains(value) then (seen, index :: duplicates) else (seen + value, duplicates)

    if duplicates.isEmpty then collected.valid
    else
      Violations(
        Violation(Constraint.Collection.Unique, Data.Array(duplicates.reverse), hint = None)
      ).invalid

object CollectionDecoder:
  private val empty: Violations = Violations(
    Violation(Constraint.Collection.Minimum(Comparison(1L, exclusive = false)), actual = 0L, hint = None)
  )
