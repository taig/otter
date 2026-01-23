package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.BranchOperation

import scala.annotation.targetName

trait BranchComponent[F[_], G[_]](using F: BranchOperation[F, G]):
  def apply[A](name: String, schema: => G[A]): F[A] = F.lift(name, schema = Reference.later(schema))

object BranchComponent:
  trait Read[F[_], G[_]](using F: BranchOperation.Read[F, G]):
    @targetName("applyRead")
    def apply[A](name: String, schema: => G[A]): F[A] = F.lift(name, schema = Reference.later(schema))

  trait Write[F[_], G[_]](using F: BranchOperation.Write[F, G]):
    @targetName("applyWrite")
    def apply[A](name: String, schema: => G[A]): F[A] = F.lift(name, schema = Reference.later(schema))
