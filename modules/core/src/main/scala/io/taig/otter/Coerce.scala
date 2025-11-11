package io.taig.otter

trait Coerce[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def coerce[H[a] <: G[a], A](schema: Reference[H, A]): F[H, A]

  def schema[H[a] <: G[a], A](self: F[H, A]): Reference[H, ?]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Coerce[H, G] = new Coerce[H, G]:
    override def coerce[I[a] <: G[a], A](schema: Reference[I, A]): H[I, A] = fK(self.coerce(schema))

    override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

object Coerce:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Coerce[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Coerce.Read[H, G] = new Read[H, G]:
      override def coerce[I[a] <: G[a], A](schema: Reference[I, A]): H[I, A] = fK(self.coerce(schema))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Coerce.Read[F, G]): Coerce.Read[F, G] = self

    given InvariantK2[Coerce.Read] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Coerce.Read[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Coerce.Read[I, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Coerce[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Coerce.Write[H, G] = new Write[H, G]:
      override def coerce[I[a] <: G[a], A](schema: Reference[I, A]): H[I, A] = fK(self.coerce(schema))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Coerce.Write[F, G]): Coerce.Write[F, G] = self

    given InvariantK2[Coerce.Write] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Coerce.Write[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Coerce.Write[I, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Coerce[F, G]): Coerce[F, G] = self

  given InvariantK2[Coerce] with
    extension [H[+_[a] <: G[a], _], G[_]](fa: Coerce[H, G])
      override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
          gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
      ): Coerce[I, G] = fa.imapK(fK)(gK)

  given derive[G[+_[a] <: I[a], _], H[+_[a] <: I[a], _], I[_]](using
      W: WrapperK2[G, H, I],
      F: Coerce[H, I]
  ): Coerce[G, I] = F.imapK[G]([s[a] <: I[a], a] => (gsa: H[s, a]) => W.inject(gsa))([s[a] <: I[a], a] =>
    (hsa: G[s, a]) => W.extract(hsa)
  )
