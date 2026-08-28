package io.taig.otter.operation

import cats.data.NonEmptyChain
import io.taig.otter.Reference

/** Constructs the union type `F` from branches of type `G`. */
trait UnionOperation[F[- _, + _], G[- _, + _]]:
  def lift[W, R](branch: Reference[G, W, R]): F[W, R]

  extension [W, R](fa: F[W, R]) def branches: NonEmptyChain[Reference[G, ?, ?]]

object UnionOperation:
  inline def apply[F[- _, + _], G[- _, + _]](using self: UnionOperation[F, G]): UnionOperation[F, G] = self

/** Lets a single branch stand in for the union that holds it, so that a chain can start with `branch(...)`. */
trait UnionableOperation[F[- _, + _], G[- _, + _]]:
  extension [W, R](fa: F[W, R]) def toUnion: G[W, R]

object UnionableOperation:
  inline def apply[F[- _, + _], G[- _, + _]](using self: UnionableOperation[F, G]): UnionableOperation[F, G] = self

  def derived[F[- _, + _], G[- _, + _]](using G: UnionOperation[G, F]): UnionableOperation[F, G] =
    new UnionableOperation[F, G]:
      extension [W, R](fa: F[W, R]) override def toUnion: G[W, R] = G.lift(Reference.now(fa))

  /** A union is already a union. Lets one `:+` serve both `branch :+ branch` and `union :+ branch`. */
  def identity[F[- _, + _]]: UnionableOperation[F, F] = new UnionableOperation[F, F]:
    extension [W, R](fa: F[W, R]) override def toUnion: F[W, R] = fa
