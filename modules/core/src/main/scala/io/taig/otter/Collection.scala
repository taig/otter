package io.taig.otter

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.validation.Validation

trait Collection[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def chained[H[a] <: G[a], A](
      schema: Reference[H, A],
      validation: Validation[Constraint.Collection, Chain[A]]
  ): F[H, Chain[A]]

  def linked[H[a] <: G[a], A](
      schema: Reference[H, A],
      validation: Validation[Constraint.Collection, List[A]]
  ): F[H, List[A]]

  def indexed[H[a] <: G[a], A](
      schema: Reference[H, A],
      validation: Validation[Constraint.Collection, Vector[A]]
  ): F[H, Vector[A]]

  def schema[H[a] <: G[a], A](self: F[H, A]): Reference[H, ?]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Collection[H, G] = new Collection[H, G]:
    override def chained[I[a] <: G[a], A](
        schema: Reference[I, A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): H[I, Chain[A]] = fK(self.chained(schema, validation))

    override def linked[I[a] <: G[a], A](
        schema: Reference[I, A],
        validation: Validation[Constraint.Collection, List[A]]
    ): H[I, List[A]] = fK(self.linked(schema, validation))

    override def indexed[I[a] <: G[a], A](
        schema: Reference[I, A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): H[I, Vector[A]] = fK(self.indexed(schema, validation))

    override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

object Collection:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Collection[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Collection.Read[H, G] = new Read[H, G]:
      override def chained[I[a] <: G[a], A](
          schema: Reference[I, A],
          validation: Validation[Constraint.Collection, Chain[A]]
      ): H[I, Chain[A]] = fK(self.chained(schema, validation))

      override def linked[I[a] <: G[a], A](
          schema: Reference[I, A],
          validation: Validation[Constraint.Collection, List[A]]
      ): H[I, List[A]] = fK(self.linked(schema, validation))

      override def indexed[I[a] <: G[a], A](
          schema: Reference[I, A],
          validation: Validation[Constraint.Collection, Vector[A]]
      ): H[I, Vector[A]] = fK(self.indexed(schema, validation))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Collection.Read[F, G]): Collection.Read[F, G] = self

    given InvariantK2[Collection.Read] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Collection.Read[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Collection.Read[H, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Collection[F, G]:
    self =>

    def chained[H[a] <: G[a], A](schema: Reference[H, A]): F[H, Chain[A]]

    override def chained[H[a] <: G[a], A](
        schema: Reference[H, A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): F[H, Chain[A]] = chained(schema, validation = Validation.valid)

    def linked[H[a] <: G[a], A](schema: Reference[H, A]): F[H, List[A]]

    override def linked[H[a] <: G[a], A](
        schema: Reference[H, A],
        validation: Validation[Constraint.Collection, List[A]]
    ): F[H, List[A]] = linked(schema, validation = Validation.valid)

    def indexed[H[a] <: G[a], A](schema: Reference[H, A]): F[H, Vector[A]]

    override def indexed[H[a] <: G[a], A](
        schema: Reference[H, A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): F[H, Vector[A]] = indexed(schema, validation = Validation.valid)

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Collection.Write[H, G] = new Write[H, G]:
      override def chained[I[a] <: G[a], A](schema: Reference[I, A]): H[I, Chain[A]] =
        fK(self.chained(schema))

      override def linked[I[a] <: G[a], A](schema: Reference[I, A]): H[I, List[A]] =
        fK(self.linked(schema))

      override def indexed[I[a] <: G[a], A](schema: Reference[I, A]): H[I, Vector[A]] =
        fK(self.indexed(schema))

      override def schema[I[a] <: G[a], A](hia: H[I, A]): Reference[I, ?] = self.schema(gK(hia))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Collection.Write[F, G]): Collection.Write[F, G] = self

    given InvariantK2[Collection.Write] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Collection.Write[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Collection.Write[H, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Collection[F, G]): Collection[F, G] = self

  given InvariantK2[Collection] with
    extension [F[+_[a] <: G[a], _], G[_]](fa: Collection[F, G])
      override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
          gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
      ): Collection[H, G] = fa.imapK(fK)(gK)
