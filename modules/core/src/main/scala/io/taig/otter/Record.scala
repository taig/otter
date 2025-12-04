package io.taig.otter

trait Record[F[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]]:
  self =>

  def apply[I[a] <: H[a], A](field: G[I, A]): F[G[I, *], A]

  def empty: F[G[H, *], Unit]

  def imapK[J[+_[a] <: G[H, a], _]](fK: [S[a] <: G[H, a], A] => F[S, A] => J[S, A])(
      gK: [S[a] <: G[H, a], A] => J[S, A] => F[S, A]
  ): Record[J, G, H] = new Record[J, G, H]:
    override def apply[I[a] <: H[a], A](field: G[I, A]): J[G[I, *], A] = fK(self.apply(field))
    override def empty: J[G[H, *], Unit] = fK(self.empty)

  // extension [J[a] <: H[a], A](fja: F[J, A])
  //   def fields: Chain[Reference[J, ?]]

  //   def zip[K[a] <: H[a], B](schema: F[K, B]): F[[a] =>> J[a] | K[a], (A, B)]

  //   final def *[K[a] <: H[a], B](schema: F[K, B])(using
  //       merge: Merge[A, B]
  //   )(using Invariant[F[[a] =>> J[a] | K[a], *]]): F[[a] =>> J[a] | K[a], merge.Out] =
  //     zip(schema).imap(merge.apply)(merge.unapply)

  //   final def :*[K[a] <: H[a], B](schema: F[K, B])(using
  //       append: Append[A, B]
  //   )(using Invariant[F[[a] =>> J[a] | K[a], *]]): F[[a] =>> J[a] | K[a], append.Out] =
  //     zip(schema).imap(append.apply)(append.unapply)

  //   final def *:[K[a] <: H[a], B](schema: F[K, B])(using
  //       prepend: Prepend[A, B]
  //   )(using Invariant[F[[a] =>> J[a] | K[a], *]]): F[[a] =>> J[a] | K[a], prepend.Out] =
  //     zip(schema).imap(prepend.apply)(prepend.unapply)

object Record:
  trait Read[F[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]] extends Record[F, G, H]:
    self =>

    override def imapK[J[+_[a] <: G[H, a], _]](fK: [S[a] <: G[H, a], A] => F[S, A] => J[S, A])(
        gK: [S[a] <: G[H, a], A] => J[S, A] => F[S, A]
    ): Record.Read[J, G, H] = new Read[J, G, H]:
      override def apply[I[a] <: H[a], A](field: G[I, A]): J[G[I, *], A] = fK(self.apply(field))
      override def empty: J[G[H, *], Unit] = fK(self.empty)

  object Read:
    inline def apply[F[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Record.Read[F, G, H]
    ): Record.Read[F, G, H] = self

    given InvariantK3[Record.Read] with
      extension [I[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]](fa: Record.Read[I, G, H])
        def imapK[J[+_[a] <: G[H, a], _]](fK: [S[a] <: G[H, a], A] => I[S, A] => J[S, A])(
            gK: [S[a] <: G[H, a], A] => J[S, A] => I[S, A]
        ): Record.Read[J, G, H] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]] extends Record[F, G, H]:
    self =>

    override def imapK[J[+_[a] <: G[H, a], _]](fK: [S[a] <: G[H, a], A] => F[S, A] => J[S, A])(
        gK: [S[a] <: G[H, a], A] => J[S, A] => F[S, A]
    ): Record.Write[J, G, H] = new Write[J, G, H]:
      override def apply[I[a] <: H[a], A](field: G[I, A]): J[G[I, *], A] = fK(self.apply(field))
      override def empty: J[G[H, *], Unit] = fK(self.empty)

  object Write:
    inline def apply[F[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Record.Write[F, G, H]
    ): Record.Write[F, G, H] = self

    given InvariantK3[Record.Write] with
      extension [I[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]](fa: Record.Write[I, G, H])
        def imapK[J[+_[a] <: G[H, a], _]](fK: [S[a] <: G[H, a], A] => I[S, A] => J[S, A])(
            gK: [S[a] <: G[H, a], A] => J[S, A] => I[S, A]
        ): Record.Write[J, G, H] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]](using self: Record[F, G, H]): Record[F, G, H] =
    self

  given InvariantK3[Record] with
    extension [I[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]](fa: Record[I, G, H])
      def imapK[J[+_[a] <: G[H, a], _]](fK: [S[a] <: G[H, a], A] => I[S, A] => J[S, A])(
          gK: [S[a] <: G[H, a], A] => J[S, A] => I[S, A]
      ): Record[J, G, H] = fa.imapK(fK)(gK)
