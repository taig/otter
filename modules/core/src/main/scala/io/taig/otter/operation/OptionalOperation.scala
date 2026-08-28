package io.taig.otter.operation

import io.taig.otter.Reference

/** Constructs the optional type `F` over schemas of type `G`. */
trait OptionalOperation[F[- _, + _], G[- _, + _]]:
  def lift[W, R](schema: => Reference[G, W, R]): F[Option[W], Option[R]]

  def lift[W, R](schema: => Reference[G, W, R], default: => R): F[W, R]

  extension [W, R](fa: F[W, R]) def schema: Reference[G, ?, ?]

object OptionalOperation:
  inline def apply[F[- _, + _], G[- _, + _]](using self: OptionalOperation[F, G]): OptionalOperation[F, G] = self

/** Lets any schema be made optional in place. */
trait OptionalableOperation[F[- _, + _], G[- _, + _]]:
  extension [W, R](fa: F[W, R])
    def optional: G[Option[W], Option[R]]
    def optional(default: => R): G[W, R]

object OptionalableOperation:
  inline def apply[F[- _, + _], G[- _, + _]](using
      self: OptionalableOperation[F, G]
  ): OptionalableOperation[F, G] = self

  def derived[F[- _, + _], G[- _, + _]](using G: OptionalOperation[G, F]): OptionalableOperation[F, G] =
    new OptionalableOperation[F, G]:
      extension [W, R](fa: F[W, R])
        override def optional: G[Option[W], Option[R]] = G.lift(Reference.now(fa))
        override def optional(default: => R): G[W, R] = G.lift(Reference.now(fa), default)
