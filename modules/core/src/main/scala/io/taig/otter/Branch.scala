package io.taig.otter

trait Branch[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  self =>

  def apply[I[a] <: H[a], A](name: String, schema: Reference[I, A]): F[I, A]

  extension [A](self: F[H, A]) def name: String

  extension [I[a] <: H[a], A](fia: F[I, A]) def schema: Reference[I, ?]

  def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
      gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
  ): Branch[I, G, H] = new Branch[I, G, H]:
    override def apply[J[a] <: H[a], A](name: String, schema: Reference[J, A]): I[J, A] =
      fK(self.apply(name, schema))

    extension [A](iha: I[H, A]) def name: String = self.name(gK(iha))

    extension [J[a] <: H[a], A](ija: I[J, A]) def schema: Reference[J, ?] = self.schema(gK(ija))

object Branch:
  trait Read[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Branch[F, G, H]:
    self =>

    override def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
        gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
    ): Branch.Read[I, G, H] = new Read[I, G, H]:
      override def apply[J[a] <: H[a], A](name: String, schema: Reference[J, A]): I[J, A] =
        fK(self.apply(name, schema))

      extension [A](iha: I[H, A]) def name: String = self.name(gK(iha))

      extension [J[a] <: H[a], A](ija: I[J, A]) def schema: Reference[J, ?] = self.schema(gK(ija))

  object Read:
    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Branch.Read[F, G, H]
    ): Branch.Read[F, G, H] = self

    given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Branch.Read] with
      extension [H[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](self: Branch.Read[H, I, J])
        def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => H[S, A] => K[S, A])(
            gK: [S[a] <: J[a], A] => K[S, A] => H[S, A]
        ): Branch.Read[K, I, J] = self.imapK(fK)(gK)

  trait Write[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Branch[F, G, H]:
    self =>

    override def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
        gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
    ): Branch.Write[I, G, H] = new Write[I, G, H]:
      override def apply[J[a] <: H[a], A](name: String, schema: Reference[J, A]): I[J, A] =
        fK(self.apply(name, schema))

      extension [A](ia: I[H, A]) def name: String = self.name(gK(ia))

      extension [J[a] <: H[a], A](ia: I[J, A]) def schema: Reference[J, ?] = self.schema(gK(ia))

  object Write:
    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Branch.Write[F, G, H]
    ): Branch.Write[F, G, H] = self

    given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Branch.Write] with
      extension [H[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](self: Branch.Write[H, I, J])
        def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => H[S, A] => K[S, A])(
            gK: [S[a] <: J[a], A] => K[S, A] => H[S, A]
        ): Branch.Write[K, I, J] = self.imapK(fK)(gK)

  inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using self: Branch[F, G, H]): Branch[F, G, H] = self

  given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Branch] with
    extension [H[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](self: Branch[H, I, J])
      def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => H[S, A] => K[S, A])(
          gK: [S[a] <: J[a], A] => K[S, A] => H[S, A]
      ): Branch[K, I, J] = self.imapK(fK)(gK)
