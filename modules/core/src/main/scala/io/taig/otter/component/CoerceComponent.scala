package io.taig.otter.component

import io.taig.otter.operation.CoerceOperation
import io.taig.otter.Reference
import scala.annotation.targetName

trait CoerceComponent[F[_], G[_]](using F: CoerceOperation[F, G]):
  def coerce[A](schema: G[A]): F[A] = F.lift(schema = Reference.later(schema))

object CoerceComponent:
  trait Read[F[_], G[_]](using F: CoerceOperation.Read[F, G]):
    @targetName("coerceRead")
    def coerce[A](schema: G[A]): F[A] = F.lift(schema = Reference.later(schema))

  trait Write[F[_], G[_]](using F: CoerceOperation.Write[F, G]):
    @targetName("coerceWrite")
    def coerce[A](schema: G[A]): F[A] = F.lift(schema = Reference.later(schema))
