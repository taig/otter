package io.taig.otter.component

import io.taig.otter.operation.CollectionOperation
import io.taig.validation.std
import io.taig.validation.Validation
import io.taig.otter.Constraint
import cats.data.NonEmptyList
import cats.Invariant
import cats.syntax.all.*

trait CollectionComponent[Shape[_], Self[_[a] <: Shape[a], _]](using operation: CollectionOperation[Shape, Self]):
  object collection:
    def list[Value[a] <: Shape[a], A](schema: => Value[A]): Self[Value, List[A]] =
      operation.linked(schema, Validation.valid)

// trait CollectionComponent[Self[_]: Invariant, -Value[_]](using operation: CollectionOperation[Self, Value]):
//   object collection:
//     def list[A](
//         schema: => Value[A],
//         validation: Validation[Constraint.Collection, List[A]]
//     ): Self[List[A]] = operation.linked(schema, validation)

//     final def list[A](schema: => Value[A]): Self[List[A]] =
//       list(schema, validation = Validation.valid)

//     def nonEmptyList[A](
//         schema: => Value[A],
//         validation: Validation[Constraint.Collection, List[A]]
//     ): Self[NonEmptyList[A]] = operation
//       .linked(schema, validation = validation.and(std.collection.list[A].minimum(reference = 1)))
//       .imap(NonEmptyList.fromListUnsafe)(_.toList)
