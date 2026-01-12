package io.taig.otter.component

import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation
import io.taig.otter.Constraint
import io.taig.otter.Reference
import scala.annotation.targetName

trait CollectionComponent[F[_], G[_]](using F: CollectionOperation[F, G]):
  final def list[A](schema: => G[A], validation: Validation[Constraint.Collection, List[A]]): F[List[A]] =
    F.list(schema = Reference.later(schema), validation)

  final def list[A](schema: => G[A]): F[List[A]] = list(schema, validation = Validation.valid)

object CollectionComponent:
  trait Read[F[_], G[_]](using F: CollectionOperation.Read[F, G]):
    @targetName("listRead")
    final def list[A](schema: => G[A], validation: Validation[Constraint.Collection, List[A]]): F[List[A]] =
      F.list(schema = Reference.later(schema), validation)

    @targetName("listRead")
    final def list[A](schema: => G[A]): F[List[A]] = list(schema, validation = Validation.valid)

  trait Write[F[_], G[_]](using F: CollectionOperation.Write[F, G]):
    @targetName("listWrite")
    final def list[A](schema: => G[A], validation: Validation[Constraint.Collection, List[A]]): F[List[A]] =
      F.list(schema = Reference.later(schema), validation)

    @targetName("listWrite")
    final def list[A](schema: => G[A]): F[List[A]] = list(schema, validation = Validation.valid)
