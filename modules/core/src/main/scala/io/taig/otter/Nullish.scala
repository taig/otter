package io.taig.otter

import cats.Eval

trait Nullish[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def nullable[H[a] <: G[a], A](schema: Reference[H, A]): F[H, Option[A]]

  def nullable[H[a] <: G[a], A](schema: Reference[H, A], default: Eval[A]): F[H, A]

  def schema[H[a] <: G[a], A](self: F[H, A]): Reference[H, ?]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Nullish[H, G] = new Nullish[H, G]:
    override def nullable[I[a] <: G[a], A](schema: Reference[I, A]): H[I, Option[A]] = fK(self.nullable(schema))

    override def nullable[I[a] <: G[a], A](schema: Reference[I, A], default: Eval[A]): H[I, A] =
      fK(self.nullable(schema, default))

    override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

object Nullish:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Nullish[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Nullish.Read[H, G] = new Read[H, G]:
      override def nullable[I[a] <: G[a], A](schema: Reference[I, A]): H[I, Option[A]] = fK(self.nullable(schema))

      override def nullable[I[a] <: G[a], A](schema: Reference[I, A], default: Eval[A]): H[I, A] =
        fK(self.nullable(schema, default))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Nullish.Read[F, G]): Nullish.Read[F, G] = self

    given InvariantK2[Nullish.Read] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Nullish.Read[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Nullish.Read[I, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Nullish[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Nullish.Write[H, G] = new Write[H, G]:
      override def nullable[I[a] <: G[a], A](schema: Reference[I, A]): H[I, Option[A]] = fK(self.nullable(schema))

      override def nullable[I[a] <: G[a], A](schema: Reference[I, A], default: Eval[A]): H[I, A] =
        fK(self.nullable(schema, default))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Nullish.Write[F, G]): Nullish.Write[F, G] = self

    given InvariantK2[Nullish.Write] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Nullish.Write[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Nullish.Write[I, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Nullish[F, G]): Nullish[F, G] = self

  given InvariantK2[Nullish] with
    extension [H[+_[a] <: G[a], _], G[_]](fa: Nullish[H, G])
      override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
          gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
      ): Nullish[I, G] = fa.imapK(fK)(gK)
