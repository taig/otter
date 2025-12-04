package io.taig.otter

trait Branch[F[+_[a] <: G[a], _], G[_]]:
  self =>

  extension [A](self: F[G, A]) def name: String

  extension [H[a] <: G[a], A](self: F[H, A]) def schema: Reference[H, ?]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Branch[H, G] = new Branch[H, G]:
    extension [A](hga: H[G, A]) def name: String = self.name(gK(hga))

    extension [I[a] <: G[a], A](hia: H[I, A]) def schema: Reference[I, ?] = self.schema(gK(hia))

object Branch:
  sealed trait Read[F[+_[a] <: G[a], _], G[_]] extends Branch[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Branch.Read[H, G] = new Read[H, G]:
      extension [A](hga: H[G, A]) def name: String = self.name(gK(hga))

      extension [I[a] <: G[a], A](hia: H[I, A]) def schema: Reference[I, ?] = self.schema(gK(hia))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Branch.Read[F, G]): Branch.Read[F, G] = self

    given InvariantK2[Branch.Read] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Branch.Read[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Branch.Read[H, G] = fa.imapK(fK)(gK)

  sealed trait Write[F[+_[a] <: G[a], _], G[_]] extends Branch[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Branch.Write[H, G] = new Write[H, G]:
      extension [A](hga: H[G, A]) def name: String = self.name(gK(hga))

      extension [I[a] <: G[a], A](hia: H[I, A]) def schema: Reference[I, ?] = self.schema(gK(hia))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Branch.Write[F, G]): Branch.Write[F, G] = self

    given InvariantK2[Branch.Write] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Branch.Write[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Branch.Write[H, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Branch[F, G]): Branch[F, G] = self

  given InvariantK2[Branch] with
    extension [F[+_[a] <: G[a], _], G[_]](fa: Branch[F, G])
      override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
          gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
      ): Branch[H, G] = fa.imapK(fK)(gK)
