package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.InvariantK
import io.taig.otter.Reference
import io.taig.validation.Validation

trait CollectionOperation[F[_], G[_]]:
  self =>

  def chained[A](schema: Reference[G, A], validation: Validation[Constraint.Collection, Chain[A]]): F[Chain[A]]

  def indexed[A](schema: Reference[G, A], validation: Validation[Constraint.Collection, Vector[A]]): F[Vector[A]]

  def linked[A](schema: Reference[G, A], validation: Validation[Constraint.Collection, List[A]]): F[List[A]]

  extension [A](fa: F[A]) def schema: Reference[G, ?]

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): CollectionOperation[H, G] =
    new CollectionOperation[H, G]:
      override def chained[A](
          schema: Reference[G, A],
          validation: Validation[Constraint.Collection, Chain[A]]
      ): H[Chain[A]] = fK(self.chained(schema, validation))

      override def indexed[A](
          schema: Reference[G, A],
          validation: Validation[Constraint.Collection, Vector[A]]
      ): H[Vector[A]] = fK(self.indexed(schema, validation))

      override def linked[A](
          schema: Reference[G, A],
          validation: Validation[Constraint.Collection, List[A]]
      ): H[List[A]] = fK(self.linked(schema, validation))

      extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

object CollectionOperation:
  trait Read[F[_], G[_]] extends CollectionOperation[F, G]:
    self =>

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): CollectionOperation.Read[H, G] =
      new Read[H, G]:
        override def chained[A](
            schema: Reference[G, A],
            validation: Validation[Constraint.Collection, Chain[A]]
        ): H[Chain[A]] =
          fK(self.chained(schema, validation))

        override def indexed[A](
            schema: Reference[G, A],
            validation: Validation[Constraint.Collection, Vector[A]]
        ): H[Vector[A]] =
          fK(self.indexed(schema, validation))

        override def linked[A](
            schema: Reference[G, A],
            validation: Validation[Constraint.Collection, List[A]]
        ): H[List[A]] = fK(self.linked(schema, validation))

        extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Read:
    inline def apply[F[_], G[_]](using self: CollectionOperation.Read[F, G]): CollectionOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> CollectionOperation.Read[f, F]]:
      extension [G[_]](fa: CollectionOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): CollectionOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends CollectionOperation[F, G]:
    self =>

    def chained[A](schema: Reference[G, A]): F[Chain[A]]

    final override def chained[A](
        schema: Reference[G, A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): F[Chain[A]] = chained(schema)

    def indexed[A](schema: Reference[G, A]): F[Vector[A]]

    final override def indexed[A](
        schema: Reference[G, A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): F[Vector[A]] = indexed(schema)

    def linked[A](schema: Reference[G, A]): F[List[A]]

    final override def linked[A](
        schema: Reference[G, A],
        validation: Validation[Constraint.Collection, List[A]]
    ): F[List[A]] = linked(schema)

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): CollectionOperation.Write[H, G] =
      new Write[H, G]:
        override def chained[A](schema: Reference[G, A]): H[Chain[A]] = fK(self.chained(schema))

        override def indexed[A](schema: Reference[G, A]): H[Vector[A]] = fK(self.indexed(schema))

        override def linked[A](schema: Reference[G, A]): H[List[A]] = fK(self.linked(schema))

        extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Write:
    inline def apply[F[_], G[_]](using self: CollectionOperation.Write[F, G]): CollectionOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> CollectionOperation.Write[f, F]]:
      extension [G[_]](fa: CollectionOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): CollectionOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: CollectionOperation[F, G]): CollectionOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> CollectionOperation[f, F]]:
    extension [G[_]](fa: CollectionOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): CollectionOperation[H, F] =
        fa.imapK(fK)(gK)
