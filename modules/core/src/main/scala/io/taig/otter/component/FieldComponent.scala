package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.FieldOperation

import scala.annotation.targetName

trait FieldComponent[F[_], G[_]](using F: FieldOperation[F, G]):
  def apply[A](name: String, schema: => G[A]): F[A] = F.lift(name, schema = Reference.later(schema))

object FieldComponent:
  trait Read[F[_], G[_]](using F: FieldOperation.Read[F, G]):
    @targetName("applyRead")
    def apply[A](name: String, schema: => G[A]): F[A] = F.lift(name, schema = Reference.later(schema))

  trait Write[F[_], G[_]](using F: FieldOperation.Write[F, G]):
    @targetName("applyWrite")
    def apply[A](name: String, schema: => G[A]): F[A] = F.lift(name, schema = Reference.later(schema))
