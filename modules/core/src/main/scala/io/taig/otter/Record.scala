package io.taig.otter
import cats.data.Chain

trait Record[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def empty: F[G, Unit]

  def field[H[a] <: G[a], A](name: String, schema: Reference[H, A]): F[H, A]

  extension [A](fha: F[G, A]) def fields: Chain[Field[G, ?]]

  extension [I[a] <: G[a], A](fia: F[I, A]) def zip[J[a] <: G[a], B](schema: F[J, B]): F[[a] =>> I[a] | J[a], (A, B)]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Record[H, G] = new Record[H, G]:
    override def empty: H[G, Unit] = fK(self.empty)

    override def field[I[a] <: G[a], A](name: String, schema: Reference[I, A]): H[I, A] =
      fK(self.field(name, schema))

    extension [A](fha: H[G, A]) override def fields: Chain[Field[G, ?]] = self.fields(gK(fha))

    extension [I[a] <: G[a], A](fia: H[I, A])
      override def zip[J[a] <: G[a], B](schema: H[J, B]): H[[a] =>> I[a] | J[a], (A, B)] =
        fK(self.zip(gK(fia))(gK(schema)))

object Record:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Record[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Record.Read[H, G] = new Read[H, G]:
      override def empty: H[G, Unit] = fK(self.empty)

      override def field[I[a] <: G[a], A](name: String, schema: Reference[I, A]): H[I, A] =
        fK(self.field(name, schema))

      extension [A](fha: H[G, A]) override def fields: Chain[Field[G, ?]] = self.fields(gK(fha))

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
      override def empty: H[G, Unit] = fK(self.empty)

      override def field[I[a] <: G[a], A](name: String, schema: Reference[I, A]): H[I, A] =
        fK(self.field(name, schema))

      extension [A](fha: H[G, A]) override def fields: Chain[Field[G, ?]] = self.fields(gK(fha))

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
