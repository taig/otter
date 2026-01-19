package io.taig.otter.operation

import cats.data.NonEmptyChain
import io.taig.enumeration.ext.Mapping
import io.taig.otter.InvariantK
import io.taig.otter.Reference
import io.taig.otter.codec.Encoder

trait EnumerationOperation[F[_], G[_]]:
  self =>

  def lift[A, B](schema: Reference[G, A], mapping: Mapping[B, A]): F[B]

  extension [A](fa: F[A])
    def encode[T](encoder: Encoder[G, T]): NonEmptyChain[T]

    def schema: Reference[G, ?]

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): EnumerationOperation[H, G] =
    new EnumerationOperation[H, G]:
      override def lift[A, B](schema: Reference[G, A], mapping: Mapping[B, A]): H[B] =
        fK(self.lift(schema, mapping))

      extension [A](ha: H[A])
        override def encode[T](encoder: Encoder[G, T]): NonEmptyChain[T] = self.encode(gK(ha))(encoder)

        override def schema: Reference[G, ?] = self.schema(gK(ha))

object EnumerationOperation:
  trait Read[F[_], G[_]] extends EnumerationOperation[F, G]:
    self =>

    final override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): EnumerationOperation.Read[H, G] =
      new Read[H, G]:
        override def lift[A, B](schema: Reference[G, A], mapping: Mapping[B, A]): H[B] =
          fK(self.lift(schema, mapping))

        extension [A](ha: H[A])
          override def encode[T](encoder: Encoder[G, T]): NonEmptyChain[T] = self.encode(gK(ha))(encoder)

          override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Read:
    inline def apply[F[_], G[_]](using self: EnumerationOperation.Read[F, G]): EnumerationOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> EnumerationOperation.Read[f, F]]:
      extension [G[_]](fa: EnumerationOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): EnumerationOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends EnumerationOperation[F, G]:
    self =>

    final override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): EnumerationOperation.Write[H, G] =
      new Write[H, G]:
        override def lift[A, B](schema: Reference[G, A], mapping: Mapping[B, A]): H[B] =
          fK(self.lift(schema, mapping))

        extension [A](ha: H[A])
          override def encode[T](encoder: Encoder[G, T]): NonEmptyChain[T] = self.encode(gK(ha))(encoder)

          override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Write:
    inline def apply[F[_], G[_]](using self: EnumerationOperation.Write[F, G]): EnumerationOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> EnumerationOperation.Write[f, F]]:
      extension [G[_]](fa: EnumerationOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): EnumerationOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: EnumerationOperation[F, G]): EnumerationOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> EnumerationOperation[f, F]]:
    extension [G[_]](fa: EnumerationOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): EnumerationOperation[H, F] =
        fa.imapK(fK)(gK)
