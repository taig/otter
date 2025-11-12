package io.taig.otter
import cats.Eq

trait Constant[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def constant[H[a] <: G[a], A](schema: Reference[H, A], value: A, eq: Eq[A]): F[H, A]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Constant[H, G] = new Constant[H, G]:
    override def constant[I[a] <: G[a], A](schema: Reference[I, A], value: A, eq: Eq[A]): H[I, A] =
      fK(self.constant(schema, value, eq))

object Constant:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Constant[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Constant.Read[H, G] = new Read[H, G]:
      override def constant[I[a] <: G[a], A](schema: Reference[I, A], value: A, eq: Eq[A]): H[I, A] =
        fK(self.constant(schema, value, eq))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Constant.Read[F, G]): Constant.Read[F, G] = self

    given InvariantK2[Constant.Read] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Constant.Read[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Constant.Read[H, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Constant[F, G]:
    self =>

    def constant[H[a] <: G[a], A](schema: Reference[H, A], value: A): F[H, A]

    final override def constant[H[a] <: G[a], A](schema: Reference[H, A], value: A, eq: Eq[A]): F[H, A] =
      constant(schema, value)

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Constant.Write[H, G] = new Write[H, G]:
      override def constant[I[a] <: G[a], A](schema: Reference[I, A], value: A): H[I, A] =
        fK(self.constant(schema, value))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Constant.Write[F, G]): Constant.Write[F, G] = self

    given InvariantK2[Constant.Write] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Constant.Write[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Constant.Write[H, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Constant[F, G]): Constant[F, G] = self

  given InvariantK2[Constant] with
    extension [F[+_[a] <: G[a], _], G[_]](fa: Constant[F, G])
      override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
          gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
      ): Constant[H, G] = fa.imapK(fK)(gK)
