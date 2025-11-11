package io.taig.otter

import io.taig.enumeration.ext.Mapping

trait Enumeration[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def enumeration[H[a] <: G[a], A, B](schema: Reference[H, A], mapping: Mapping[B, A]): F[H, B]

  def schema[H[a] <: G[a], A](self: F[H, A]): Reference[H, ?]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Enumeration[H, G] = new Enumeration[H, G]:
    override def enumeration[I[a] <: G[a], A, B](schema: Reference[I, A], mapping: Mapping[B, A]): H[I, B] =
      fK(self.enumeration(schema, mapping))

    override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

object Enumeration:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Enumeration[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Enumeration.Read[H, G] = new Read[H, G]:
      override def enumeration[I[a] <: G[a], A, B](schema: Reference[I, A], mapping: Mapping[B, A]): H[I, B] =
        fK(self.enumeration(schema, mapping))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Enumeration.Read[F, G]): Enumeration.Read[F, G] = self

    given InvariantK2[Enumeration.Read] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Enumeration.Read[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Enumeration.Read[I, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Enumeration[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Enumeration.Write[H, G] = new Write[H, G]:
      override def enumeration[I[a] <: G[a], A, B](schema: Reference[I, A], mapping: Mapping[B, A]): H[I, B] =
        fK(self.enumeration(schema, mapping))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Enumeration.Write[F, G]): Enumeration.Write[F, G] = self

    given InvariantK2[Enumeration.Write] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Enumeration.Write[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Enumeration.Write[I, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Enumeration[F, G]): Enumeration[F, G] = self

  given InvariantK2[Enumeration] with
    extension [H[+_[a] <: G[a], _], G[_]](fa: Enumeration[H, G])
      override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
          gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
      ): Enumeration[I, G] = fa.imapK(fK)(gK)

  given derive[G[+_[a] <: I[a], _], H[+_[a] <: I[a], _], I[_]](using
      W: Wrapper[G, H, I],
      F: Enumeration[H, I]
  ): Enumeration[G, I] = F.imapK[G]([s[a] <: I[a], a] => (gsa: H[s, a]) => W.inject(gsa))([s[a] <: I[a], a] =>
    (hsa: G[s, a]) => W.extract(hsa)
  )
