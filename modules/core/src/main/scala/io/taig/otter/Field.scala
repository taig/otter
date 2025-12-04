package io.taig.otter

import cats.Eval

trait Field[F[+_[a] <: G[a], _], G[_]]:
  self =>

  extension [A](self: F[G, A]) def name: String

  extension [H[a] <: G[a], A](self: F[H, A])
    def optional: F[H, Option[A]]

    def optional(default: Eval[A]): F[H, A]

    final def optional(default: => A): F[H, A] = optional(default = Eval.later(default))

    def schema: Reference[H, ?]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Field[H, G] = new Field[H, G]:
    extension [A](hga: H[G, A]) def name: String = self.name(gK(hga))

    extension [I[a] <: G[a], A](hia: H[I, A])
      def optional: H[I, Option[A]] = fK(self.optional(gK(hia)))

      def optional(default: Eval[A]): H[I, A] = fK(self.optional(gK(hia))(default))

      def schema: Reference[I, ?] = self.schema(gK(hia))

object Field:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Field[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Field.Read[H, G] = new Read[H, G]:
      extension [A](hga: H[G, A]) def name: String = self.name(gK(hga))

      extension [I[a] <: G[a], A](hia: H[I, A])
        def optional: H[I, Option[A]] = fK(self.optional(gK(hia)))

        def optional(default: Eval[A]): H[I, A] = fK(self.optional(gK(hia))(default))

        def schema: Reference[I, ?] = self.schema(gK(hia))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Field.Read[F, G]): Field.Read[F, G] = self

    given InvariantK2[Field.Read] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Field.Read[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Field.Read[H, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Field[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Field.Write[H, G] = new Write[H, G]:
      extension [A](hga: H[G, A]) def name: String = self.name(gK(hga))

      extension [I[a] <: G[a], A](hia: H[I, A])
        def optional: H[I, Option[A]] = fK(self.optional(gK(hia)))

        def optional(default: Eval[A]): H[I, A] = fK(self.optional(gK(hia))(default))

        def schema: Reference[I, ?] = self.schema(gK(hia))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Field.Write[F, G]): Field.Write[F, G] = self

    given InvariantK2[Field.Write] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Field.Write[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Field.Write[H, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Field[F, G]): Field[F, G] = self

  given InvariantK2[Field] with
    extension [F[+_[a] <: G[a], _], G[_]](fa: Field[F, G])
      override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
          gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
      ): Field[H, G] = fa.imapK(fK)(gK)
