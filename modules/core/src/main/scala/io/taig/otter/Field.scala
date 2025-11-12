package io.taig.otter

trait Field[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def field[H[a] <: G[a], A](name: String, schema: Reference[H, A]): F[H, A]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Field[H, G] = new Field[H, G]:
    override def field[I[a] <: G[a], A](name: String, schema: Reference[I, A]): H[I, A] =
      fK(self.field(name, schema))

object Field:
  sealed trait Read[F[+_[a] <: G[a], _], G[_]] extends Field[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Field.Read[H, G] = new Read[H, G]:
      override def field[I[a] <: G[a], A](name: String, schema: Reference[I, A]): H[I, A] =
        fK(self.field(name, schema))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Field.Read[F, G]): Field.Read[F, G] = self

  sealed trait Write[F[+_[a] <: G[a], _], G[_]] extends Field[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Field.Write[H, G] = new Write[H, G]:
      override def field[I[a] <: G[a], A](name: String, schema: Reference[I, A]): H[I, A] =
        fK(self.field(name, schema))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Field.Write[F, G]): Field.Write[F, G] = self

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Field[F, G]): Field[F, G] = self
