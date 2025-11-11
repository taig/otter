package io.taig.otter

import io.taig.validation.Validation

trait Dictionary[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def linked[H[a] <: G[a], A](
      schema: Reference[H, A],
      validation: Validation[Constraint.Object, List[A]]
  ): F[H, List[A]]

  def schema[H[a] <: G[a], A](self: F[H, A]): Reference[H, ?]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Dictionary[H, G] = new Dictionary[H, G]:
    override def linked[I[a] <: G[a], A](
        schema: Reference[I, A],
        validation: Validation[Constraint.Object, List[A]]
    ): H[I, List[A]] = fK(self.linked(schema, validation))

    override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

object Dictionary:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Dictionary[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Dictionary.Read[H, G] = new Read[H, G]:
      override def linked[I[a] <: G[a], A](
          schema: Reference[I, A],
          validation: Validation[Constraint.Object, List[A]]
      ): H[I, List[A]] = fK(self.linked(schema, validation))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Dictionary.Read[F, G]): Dictionary.Read[F, G] = self

    given InvariantK2[Dictionary.Read] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Dictionary.Read[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Dictionary.Read[H, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Dictionary[F, G]:
    self =>

    def linked[H[a] <: G[a], A](schema: Reference[H, A]): F[H, List[A]]

    final override def linked[H[a] <: G[a], A](
        schema: Reference[H, A],
        validation: Validation[Constraint.Object, List[A]]
    ): F[H, List[A]] = linked(schema)

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Dictionary.Write[H, G] = new Write[H, G]:
      override def linked[I[a] <: G[a], A](schema: Reference[I, A]): H[I, List[A]] = fK(self.linked(schema))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Dictionary.Write[F, G]): Dictionary.Write[F, G] = self

    given InvariantK2[Dictionary.Write] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Dictionary.Write[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Dictionary.Write[H, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Dictionary[F, G]): Dictionary[F, G] = self

  given InvariantK2[Dictionary] with
    extension [F[+_[a] <: G[a], _], G[_]](fa: Dictionary[F, G])
      override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
          gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
      ): Dictionary[H, G] = fa.imapK(fK)(gK)

  given derive[G[+_[a] <: I[a], _], H[+_[a] <: I[a], _], I[_]](using
      W: WrapperK2[G, H, I],
      F: Dictionary[H, I]
  ): Dictionary[G, I] = F.imapK[G]([s[a] <: I[a], a] => (gsa: H[s, a]) => W.inject(gsa))([s[a] <: I[a], a] =>
    (hsa: G[s, a]) => W.extract(hsa)
  )
