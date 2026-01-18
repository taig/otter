package io.taig.otter.component

import cats.Eq
import cats.Eval
import io.taig.otter.Reference
import io.taig.otter.operation.ConstantOperation

import scala.annotation.targetName

trait ConstantComponent[F[_], G[_]](using F: ConstantOperation[F, G]):
  final def apply[A](schema: => G[A], value: => A)(using eq: Eq[A]): F[Unit] =
    F.lift(schema = Reference.later(schema), value = Eval.later(value), eq)

object ConstantComponent:
  trait Read[F[_], G[_]](using F: ConstantOperation.Read[F, G]):
    @targetName("applyRead")
    final def apply[A](schema: => G[A], value: => A)(using eq: Eq[A]): F[Unit] =
      F.lift(schema = Reference.later(schema), value = Eval.later(value), eq)

  trait Write[F[_], G[_]](using F: ConstantOperation.Write[F, G]):
    @targetName("applyWrite")
    final def apply[A](schema: => G[A], value: => A): F[Unit] =
      F.lift(schema = Reference.later(schema), value = Eval.later(value))
