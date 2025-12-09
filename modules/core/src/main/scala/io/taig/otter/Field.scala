package io.taig.otter

import cats.Eval

trait Field[F[+_[_], _], G[+_[a] <: H[a], _], H[_]]:
  self =>

  def apply[I[a] <: H[a], A](name: String, schema: Reference[I, A]): F[I, A]

  extension [A](self: F[H, A]) def name: String

  extension [I[a] <: H[a], A](self: F[I, A])
    def optional: F[I, Option[A]]

    def optional(default: Eval[A]): F[I, A]

    final def optional(default: => A): F[I, A] = optional(default = Eval.later(default))

    def schema: Reference[I, ?]

object Field:
  trait Read[F[+_[_], _], G[+_[a] <: H[a], _], H[_]] extends Field[F, G, H]:
    self =>

  object Read:
    inline def apply[F[+_[_], _], G[+_[a] <: H[a], _], H[_]](using self: Field.Read[F, G, H]): Field.Read[F, G, H] = self

  trait Write[F[+_[_], _], G[+_[a] <: H[a], _], H[_]] extends Field[F, G, H]:
    self =>

    extension [I[a] <: H[a], A](fa: F[I, A])
      def optional: F[I, Option[A]]

      override final def optional(default: Eval[A]): F[I, A] = fa

  object Write:
    inline def apply[F[+_[_], _], G[+_[a] <: H[a], _], H[_]](using self: Field.Write[F, G, H]): Field.Write[F, G, H] = self

  inline def apply[F[+_[_], _], G[+_[a] <: H[a], _], H[_]](using self: Field[F, G, H]): Field[F, G, H] = self
