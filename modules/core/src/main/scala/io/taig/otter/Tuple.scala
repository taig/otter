package io.taig.otter

import cats.data.Chain

trait Tuple[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def apply[H[a] <: G[a], A](schema: Reference[H, A]): F[H, A]

  def empty: F[G, Unit]

  extension [H[a] <: G[a], A](fha: F[H, A])
    def schemas: Chain[Reference[H, ?]]

    def zip[I[a] >: H[a] <: G[a], B](schema: F[I, B]): F[I, (A, B)]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Tuple[H, G] = new Tuple[H, G]:
    override def apply[I[a] <: G[a], A](schema: Reference[I, A]): H[I, A] =
      fK(self.apply(schema))

    override def empty: H[G, Unit] = fK(self.empty)

    extension [I[a] <: G[a], A](hia: H[I, A]) override def schemas: Chain[Reference[I, ?]] = self.schemas(gK(hia))

    extension [I[a] <: G[a], A](hia: H[I, A])
      override def zip[J[a] >: I[a] <: G[a], B](schema: H[J, B]): H[J, (A, B)] =
        fK(self.zip(gK(hia))(gK(schema)))

object Tuple:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Tuple[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Tuple.Read[H, G] = new Read[H, G]:
      override def apply[I[a] <: G[a], A](schema: Reference[I, A]): H[I, A] =
        fK(self.apply(schema))

      override def empty: H[G, Unit] = fK(self.empty)

      extension [I[a] <: G[a], A](hia: H[I, A])
        override def schemas: Chain[Reference[I, ?]] = self.schemas(gK(hia))

        override def zip[J[a] >: I[a] <: G[a], B](schema: H[J, B]): H[J, (A, B)] =
          fK(self.zip(gK(hia))(gK(schema)))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Tuple.Read[F, G]): Tuple.Read[F, G] = self

    given InvariantK2[Tuple.Read] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Tuple.Read[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Tuple.Read[H, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Tuple[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Tuple.Write[H, G] = new Write[H, G]:
      override def apply[I[a] <: G[a], A](schema: Reference[I, A]): H[I, A] =
        fK(self.apply(schema))

      override def empty: H[G, Unit] = fK(self.empty)

      extension [I[a] <: G[a], A](hia: H[I, A])
        override def schemas: Chain[Reference[I, ?]] = self.schemas(gK(hia))

        override def zip[J[a] >: I[a] <: G[a], B](schema: H[J, B]): H[J, (A, B)] =
          fK(self.zip(gK(hia))(gK(schema)))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Tuple.Write[F, G]): Tuple.Write[F, G] = self

    given InvariantK2[Tuple.Write] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Tuple.Write[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Tuple.Write[H, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Tuple[F, G]): Tuple[F, G] = self

  given InvariantK2[Tuple] with
    extension [F[+_[a] <: G[a], _], G[_]](fa: Tuple[F, G])
      override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
          gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
      ): Tuple[H, G] = fa.imapK(fK)(gK)
