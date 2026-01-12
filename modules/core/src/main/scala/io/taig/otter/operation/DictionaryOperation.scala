package io.taig.otter.operation

import io.taig.otter.Constraint
import io.taig.otter.InvariantK
import io.taig.otter.Reference
import io.taig.validation.Validation
import scala.collection.immutable.SortedMap

trait DictionaryOperation[F[_], G[_]]:
  self =>

  def hashed[A](
      schema: Reference[G, A],
      validation: Validation[Constraint.Object, SortedMap[String, A]]
  ): F[SortedMap[String, A]]

  def linked[A](
      schema: Reference[G, A],
      validation: Validation[Constraint.Object, List[(String, A)]]
  ): F[List[(String, A)]]

  extension [A](fa: F[A]) def schema: Reference[G, ?]

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): DictionaryOperation[H, G] =
    new DictionaryOperation[H, G]:
      override def hashed[A](
          schema: Reference[G, A],
          validation: Validation[Constraint.Object, SortedMap[String, A]]
      ): H[SortedMap[String, A]] =
        fK(self.hashed(schema, validation))

      override def linked[A](
          schema: Reference[G, A],
          validation: Validation[Constraint.Object, List[(String, A)]]
      ): H[List[(String, A)]] =
        fK(self.linked(schema, validation))

      extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

object DictionaryOperation:
  trait Read[F[_], G[_]] extends DictionaryOperation[F, G]:
    self =>

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): DictionaryOperation.Read[H, G] =
      new Read[H, G]:
        override def hashed[A](
            schema: Reference[G, A],
            validation: Validation[Constraint.Object, SortedMap[String, A]]
        ): H[SortedMap[String, A]] =
          fK(self.hashed(schema, validation))

        override def linked[A](
            schema: Reference[G, A],
            validation: Validation[Constraint.Object, List[(String, A)]]
        ): H[List[(String, A)]] =
          fK(self.linked(schema, validation))

        extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Read:
    inline def apply[F[_], G[_]](using self: DictionaryOperation.Read[F, G]): DictionaryOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> DictionaryOperation.Read[f, F]]:
      extension [G[_]](fa: DictionaryOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): DictionaryOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends DictionaryOperation[F, G]:
    self =>

    def linked[A](schema: Reference[G, A]): F[List[(String, A)]]

    final override def linked[A](
        schema: Reference[G, A],
        validation: Validation[Constraint.Object, List[(String, A)]]
    ): F[List[(String, A)]] = linked(schema)

    def hashed[A](schema: Reference[G, A]): F[SortedMap[String, A]]

    final override def hashed[A](
        schema: Reference[G, A],
        validation: Validation[Constraint.Object, SortedMap[String, A]]
    ): F[SortedMap[String, A]] =
      hashed(schema)

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): DictionaryOperation.Write[H, G] =
      new Write[H, G]:
        override def hashed[A](schema: Reference[G, A]): H[SortedMap[String, A]] = fK(self.hashed(schema))

        override def linked[A](schema: Reference[G, A]): H[List[(String, A)]] = fK(self.linked(schema))

        extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Write:
    inline def apply[F[_], G[_]](using self: DictionaryOperation.Write[F, G]): DictionaryOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> DictionaryOperation.Write[f, F]]:
      extension [G[_]](fa: DictionaryOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): DictionaryOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: DictionaryOperation[F, G]): DictionaryOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> DictionaryOperation[f, F]]:
    extension [G[_]](fa: DictionaryOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): DictionaryOperation[H, F] =
        fa.imapK(fK)(gK)
