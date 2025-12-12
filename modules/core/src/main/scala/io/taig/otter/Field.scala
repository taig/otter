package io.taig.otter

import cats.Eval

trait Field[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  self =>

  def apply[I[a] <: H[a], A](name: String, schema: Reference[I, A]): F[I, A]

  extension [A](self: F[H, A]) def name: String

  extension [I[a] <: H[a], A](self: F[I, A])
    def optional: F[I, Option[A]]

    def optional(default: Eval[A]): F[I, A]

    final def optional(default: => A): F[I, A] = optional(default = Eval.later(default))

    def schema: Reference[I, ?]

  def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
      gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
  ): Field[I, G, H] = new Field[I, G, H]:
    override def apply[J[a] <: H[a], A](name: String, schema: Reference[J, A]): I[J, A] =
      fK(self.apply(name, schema))

    extension [A](ia: I[H, A]) def name: String = self.name(gK(ia))

    extension [J[a] <: H[a], A](ia: I[J, A])
      def optional: I[J, Option[A]] = fK(self.optional(gK(ia)))

      def optional(default: Eval[A]): I[J, A] = fK(self.optional(gK(ia))(default))

      def schema: Reference[J, ?] = self.schema(gK(ia))

object Field:
  given [G[+_[a] <: H[a], _], H[_]]: InvariantK3[[f[+_[a] <: h[a], _], g[+_[a] <: h[a], _], h[_]] =>> Field[f, g, h]]
  with
    extension [F[+_[a] <: HH[a], _], GG[+_[a] <: HH[a], _], HH[_]](fa: Field[F, GG, HH])
      def imapK[I[+_[a] <: HH[a], _]](fK: [S[a] <: HH[a], A] => F[S, A] => I[S, A])(
          gK: [S[a] <: HH[a], A] => I[S, A] => F[S, A]
      ): Field[I, GG, HH] = fa.imapK(fK)(gK)

  trait Read[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Field[F, G, H]:
    self =>

    override def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
        gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
    ): Field.Read[I, G, H] = new Read[I, G, H]:
      override def apply[J[a] <: H[a], A](name: String, schema: Reference[J, A]): I[J, A] =
        fK(self.apply(name, schema))

      extension [A](ia: I[H, A]) def name: String = self.name(gK(ia))

      extension [J[a] <: H[a], A](ia: I[J, A])
        def optional: I[J, Option[A]] = fK(self.optional(gK(ia)))

        def optional(default: Eval[A]): I[J, A] = fK(self.optional(gK(ia))(default))

        def schema: Reference[J, ?] = self.schema(gK(ia))

  object Read:
    given [G[+_[a] <: H[a], _], H[_]]: InvariantK3[Field.Read] with
      extension [F[+_[a] <: HH[a], _], GG[+_[a] <: HH[a], _], HH[_]](fa: Field.Read[F, GG, HH])
        def imapK[I[+_[a] <: HH[a], _]](fK: [S[a] <: HH[a], A] => F[S, A] => I[S, A])(
            gK: [S[a] <: HH[a], A] => I[S, A] => F[S, A]
        ): Field.Read[I, GG, HH] = fa.imapK(fK)(gK)

    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Field.Read[F, G, H]
    ): Field.Read[F, G, H] =
      self

  trait Write[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Field[F, G, H]:
    self =>

    extension [I[a] <: H[a], A](fa: F[I, A])
      def optional: F[I, Option[A]]

      final override def optional(default: Eval[A]): F[I, A] = fa

    override def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
        gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
    ): Field.Write[I, G, H] = new Write[I, G, H]:
      override def apply[J[a] <: H[a], A](name: String, schema: Reference[J, A]): I[J, A] =
        fK(self.apply(name, schema))

      extension [A](ia: I[H, A]) def name: String = self.name(gK(ia))

      extension [J[a] <: H[a], A](ia: I[J, A])
        def optional: I[J, Option[A]] = fK(self.optional(gK(ia)))

        def schema: Reference[J, ?] = self.schema(gK(ia))

  object Write:
    given [G[+_[a] <: H[a], _], H[_]]
        : InvariantK3[[f[+_[a] <: h[a], _], g[+_[a] <: h[a], _], h[_]] =>> Field.Write[f, g, h]] with
      extension [F[+_[a] <: HH[a], _], GG[+_[a] <: HH[a], _], HH[_]](fa: Field.Write[F, GG, HH])
        def imapK[I[+_[a] <: HH[a], _]](fK: [S[a] <: HH[a], A] => F[S, A] => I[S, A])(
            gK: [S[a] <: HH[a], A] => I[S, A] => F[S, A]
        ): Field.Write[I, GG, HH] = fa.imapK(fK)(gK)

    inline def apply[F[+_[_], _], G[+_[a] <: H[a], _], H[_]](using self: Field.Write[F, G, H]): Field.Write[F, G, H] =
      self

  inline def apply[F[+_[_], _], G[+_[a] <: H[a], _], H[_]](using self: Field[F, G, H]): Field[F, G, H] = self
