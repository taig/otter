package io.taig.otter

import cats.data.Chain
import cats.Invariant
import cats.syntax.all.*

trait Record[F[+_[a] <: G[a], _], G[_]]:
  self =>

  // def apply[H[a] <: G[a], A](field: Field[H, A]): F[H, A]

  def empty: F[G, Unit]

  extension [A](fha: F[G, A]) def fields: Chain[Reference[G, ?]]

  extension [I[a] <: G[a], A](fia: F[I, A])
    def zip[J[a] <: G[a], B](schema: F[J, B]): F[[a] =>> I[a] | J[a], (A, B)]

    final def *[J[a] <: G[a], B](schema: F[J, B])(using
        merge: Merge[A, B]
    )(using Invariant[F[[a] =>> I[a] | J[a], *]]): F[[a] =>> I[a] | J[a], merge.Out] =
      zip(schema).imap(merge.apply)(merge.unapply)

    final def :*[J[a] <: G[a], B](schema: F[J, B])(using
        append: Append[A, B]
    )(using Invariant[F[[a] =>> I[a] | J[a], *]]): F[[a] =>> I[a] | J[a], append.Out] =
      zip(schema).imap(append.apply)(append.unapply)

    final def *:[J[a] <: G[a], B](schema: F[J, B])(using
        prepend: Prepend[A, B]
    )(using Invariant[F[[a] =>> I[a] | J[a], *]]): F[[a] =>> I[a] | J[a], prepend.Out] =
      zip(schema).imap(prepend.apply)(prepend.unapply)

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Record[H, G] = new Record[H, G]:
    // override def apply[I[a] <: G[a], A](field: Field[I, A]): H[I, A] = fK(self.apply(field))

    override def empty: H[G, Unit] = fK(self.empty)

    extension [A](fha: H[G, A]) override def fields: Chain[Reference[G, ?]] = self.fields(gK(fha))

    extension [I[a] <: G[a], A](fia: H[I, A])
      override def zip[J[a] <: G[a], B](schema: H[J, B]): H[[a] =>> I[a] | J[a], (A, B)] =
        fK(self.zip(gK(fia))(gK(schema)))

object Record:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Record[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Record.Read[H, G] = new Read[H, G]:
      // override def apply[I[a] <: G[a], A](field: Field[I, A]): H[I, A] = fK(self.apply(field))

      override def empty: H[G, Unit] = fK(self.empty)

      extension [A](fha: H[G, A]) override def fields: Chain[Reference[G, ?]] = self.fields(gK(fha))

      extension [I[a] <: G[a], A](fia: H[I, A])
        override def zip[J[a] <: G[a], B](schema: H[J, B]): H[[a] =>> I[a] | J[a], (A, B)] =
          fK(self.zip(gK(fia))(gK(schema)))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Record.Read[F, G]): Record.Read[F, G] = self

    given InvariantK2[Record.Read] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Record.Read[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Record.Read[H, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Record[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Record.Write[H, G] = new Write[H, G]:
      // override def apply[I[a] <: G[a], A](field: Field[I, A]): H[I, A] = fK(self.apply(field))

      override def empty: H[G, Unit] = fK(self.empty)

      extension [A](fha: H[G, A]) override def fields: Chain[Reference[G, ?]] = self.fields(gK(fha))

      extension [I[a] <: G[a], A](fia: H[I, A])
        override def zip[J[a] <: G[a], B](schema: H[J, B]): H[[a] =>> I[a] | J[a], (A, B)] =
          fK(self.zip(gK(fia))(gK(schema)))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Record.Write[F, G]): Record.Write[F, G] = self

    given InvariantK2[Record.Write] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Record.Write[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Record.Write[H, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Record[F, G]): Record[F, G] = self

  given InvariantK2[Record] with
    extension [F[+_[a] <: G[a], _], G[_]](fa: Record[F, G])
      override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
          gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
      ): Record[H, G] = fa.imapK(fK)(gK)
