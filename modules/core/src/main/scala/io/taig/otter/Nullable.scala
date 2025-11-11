package io.taig.otter

import cats.Eval

trait Nullable[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def nullable[H[a] <: G[a], A](schema: Reference[H, A]): F[H, Option[A]]

  def nullable[H[a] <: G[a], A](schema: Reference[H, A], default: Eval[A]): F[H, A]

  def schema[H[a] <: G[a], A](self: F[H, A]): Reference[H, ?]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Nullable[H, G] = new Nullable[H, G]:
    override def nullable[I[a] <: G[a], A](schema: Reference[I, A]): H[I, Option[A]] = fK(self.nullable(schema))

    override def nullable[I[a] <: G[a], A](schema: Reference[I, A], default: Eval[A]): H[I, A] =
      fK(self.nullable(schema, default))

    override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

object Nullable:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Nullable[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Nullable.Read[H, G] = new Read[H, G]:
      override def nullable[I[a] <: G[a], A](schema: Reference[I, A]): H[I, Option[A]] = fK(self.nullable(schema))

      override def nullable[I[a] <: G[a], A](schema: Reference[I, A], default: Eval[A]): H[I, A] =
        fK(self.nullable(schema, default))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Nullable.Read[F, G]): Nullable.Read[F, G] = self

    given InvariantK2[Nullable.Read] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Nullable.Read[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Nullable.Read[I, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Nullable[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Nullable.Write[H, G] = new Write[H, G]:
      override def nullable[I[a] <: G[a], A](schema: Reference[I, A]): H[I, Option[A]] = fK(self.nullable(schema))

      override def nullable[I[a] <: G[a], A](schema: Reference[I, A], default: Eval[A]): H[I, A] =
        fK(self.nullable(schema, default))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Nullable.Write[F, G]): Nullable.Write[F, G] = self

    given InvariantK2[Nullable.Write] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Nullable.Write[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Nullable.Write[I, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Nullable[F, G]): Nullable[F, G] = self

  given InvariantK2[Nullable] with
    extension [H[+_[a] <: G[a], _], G[_]](fa: Nullable[H, G])
      override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
          gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
      ): Nullable[I, G] = fa.imapK(fK)(gK)
