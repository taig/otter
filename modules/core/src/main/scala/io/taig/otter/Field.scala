package io.taig.otter

import cats.Eval

trait Field[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  self =>

  def apply[I[a] <: H[a], A](name: String, schema: Reference[I, A]): F[I, A]

  extension [A](self: F[H, A]) def name: String

  extension [I[a] <: H[a], A](fia: F[I, A])
    def optional: F[I, Option[A]]

    def optional(default: Eval[A]): F[I, A]

    final def optional(default: => A): F[I, A] = optional(default = Eval.later(default))

    def schema: Reference[I, ?]

  def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
      gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
  ): Field[I, G, H] = new Field[I, G, H]:
    override def apply[J[a] <: H[a], A](name: String, schema: Reference[J, A]): I[J, A] =
      fK(self.apply(name, schema))

    extension [A](iha: I[H, A]) def name: String = self.name(gK(iha))

    extension [J[a] <: H[a], A](ija: I[J, A])
      def optional: I[J, Option[A]] = fK(self.optional(gK(ija)))

      def optional(default: Eval[A]): I[J, A] = fK(self.optional(gK(ija))(default))

      def schema: Reference[J, ?] = self.schema(gK(ija))

object Field:
  trait Read[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Field[F, G, H]:
    self =>

    override def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
        gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
    ): Field.Read[I, G, H] = new Read[I, G, H]:
      override def apply[J[a] <: H[a], A](name: String, schema: Reference[J, A]): I[J, A] =
        fK(self.apply(name, schema))

      extension [A](iha: I[H, A]) def name: String = self.name(gK(iha))

      extension [J[a] <: H[a], A](ija: I[J, A])
        def optional: I[J, Option[A]] = fK(self.optional(gK(ija)))

        def optional(default: Eval[A]): I[J, A] = fK(self.optional(gK(ija))(default))

        def schema: Reference[J, ?] = self.schema(gK(ija))

  object Read:
    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Field.Read[F, G, H]
    ): Field.Read[F, G, H] = self

    given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Field.Read] with
      extension [H[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](self: Field.Read[H, I, J])
        def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => H[S, A] => K[S, A])(
            gK: [S[a] <: J[a], A] => K[S, A] => H[S, A]
        ): Field.Read[K, I, J] = self.imapK(fK)(gK)

  trait Write[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Field[F, G, H]:
    self =>

    extension [I[a] <: H[a], A](fia: F[I, A])
      def optional: F[I, Option[A]]

      final override def optional(default: Eval[A]): F[I, A] = fia

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
    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Field.Write[F, G, H]
    ): Field.Write[F, G, H] = self

    given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Field.Write] with
      extension [H[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](self: Field.Write[H, I, J])
        def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => H[S, A] => K[S, A])(
            gK: [S[a] <: J[a], A] => K[S, A] => H[S, A]
        ): Field.Write[K, I, J] = self.imapK(fK)(gK)

  inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using self: Field[F, G, H]): Field[F, G, H] = self

  given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Field] with
    extension [H[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](self: Field[H, I, J])
      def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => H[S, A] => K[S, A])(
          gK: [S[a] <: J[a], A] => K[S, A] => H[S, A]
      ): Field[K, I, J] = self.imapK(fK)(gK)
