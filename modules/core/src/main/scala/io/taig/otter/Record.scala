package io.taig.otter

import cats.data.Chain

trait Record[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def empty: F[G, Unit]

  def fields[H[a] <: G[a], A](self: F[H, A]): Chain[Field[H, ?]]

  def record[H[a] <: G[a], A](field: Field[H, A]): F[H, A]

  def zip[H[a] <: G[a], A, B](left: F[H, A], right: F[H, B]): F[H, (A, B)]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Record[H, G] = new Record[H, G]:
    override def empty: H[G, Unit] = fK(self.empty)

    override def record[I[a] <: G[a], A](field: Field[I, A]): H[I, A] = fK(self.record(field))

    override def zip[I[a] <: G[a], A, B](left: H[I, A], right: H[I, B]): H[I, (A, B)] =
      fK(self.zip(gK(left), gK(right)))

    override def fields[I[a] <: G[a], A](hia: H[I, A]): Chain[Field[I, ?]] = self.fields(gK(hia))

object Record:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Record[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Record.Read[H, G] = new Read[H, G]:
      override def empty: H[G, Unit] = fK(self.empty)

      override def record[I[a] <: G[a], A](field: Field[I, A]): H[I, A] = fK(self.record(field))

      override def zip[I[a] <: G[a], A, B](left: H[I, A], right: H[I, B]): H[I, (A, B)] =
        fK(self.zip(gK(left), gK(right)))

      override def fields[I[a] <: G[a], A](hia: H[I, A]): Chain[Field[I, ?]] = self.fields(gK(hia))

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

      override def record[I[a] <: G[a], A](field: Field[I, A]): H[I, A] = fK(self.record(field))

      override def zip[I[a] <: G[a], A, B](left: H[I, A], right: H[I, B]): H[I, (A, B)] =
        fK(self.zip(gK(left), gK(right)))

      override def fields[I[a] <: G[a], A](hia: H[I, A]): Chain[Field[I, ?]] = self.fields(gK(hia))

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

  given derive[G[+_[a] <: I[a], _], H[+_[a] <: I[a], _], I[_]](using
      W: WrapperK2[G, H, I],
      F: Record[H, I]
  ): Record[G, I] = F.imapK[G]([s[a] <: I[a], a] => (gsa: H[s, a]) => W.inject(gsa))([s[a] <: I[a], a] =>
    (hsa: G[s, a]) => W.extract(hsa)
  )
