package io.taig.otter

import cats.Contravariant
import cats.Eq
import cats.Eval
import cats.Functor
import cats.Invariant
import io.taig.otter.Reference
import io.taig.otter.codec.Encoder
import io.taig.otter.operation.ConstantOperation

type Constant[+F[_], A] = Constant.Read[F, A] & Constant.Write[F, A]

object Constant:
  sealed trait Read[+F[_], +A]:
    def encode[T](encoder: Encoder[F, T]): T

    final def map[B](f: A => B): Constant.Read[F, B] = Read.Modify(self = this, f)

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Constant.Read[G, A]

    def schema: Reference[F, ?]

  object Read:
    final case class Modify[F[_], A, B](self: Constant.Read[F, A], f: A => B) extends Constant.Read[F, B]:
      export self.{encode, schema}

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Constant.Read[G, B] = copy(self = self.mapK(fK))

    given [F[_]] => Functor[Constant.Read[F, *]]:
      override def map[A, B](fa: Constant.Read[F, A])(f: A => B): Constant.Read[F, B] = fa.map(f)

    given [F[_]] => ConstantOperation.Read[Constant.Read[F, *], F]:
      override def lift[A](schema: Reference[F, A], value: Eval[A], eq: Eq[A]): Constant.Read[F, Unit] =
        Root(schema, value, eq)

      extension [A](fa: Constant.Read[F, A])
        override def encode[T](encoder: Encoder[F, T]): T = fa.encode(encoder)

        override def schema: Reference[F, ?] = fa.schema

  sealed trait Write[+F[_], -A]:
    final def contramap[B](f: B => A): Constant.Write[F, B] = Write.Modify(self = this, f)

    def encode[T](encoder: Encoder[F, T]): T

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Constant.Write[G, A]

    def schema: Reference[F, ?]

  object Write:
    final case class Modify[F[_], A, B](self: Constant.Write[F, A], f: B => A) extends Constant.Write[F, B]:
      export self.{encode, schema}

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Constant.Write[G, B] = copy(self = self.mapK(fK))

    final case class Root[F[_], A](schema: Reference[F, A], value: Eval[A]) extends Constant.Write[F, Unit]:
      override def encode[T](encoder: Encoder[F, T]): T = encoder.encode(schema.value, value.value)

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Constant.Write[G, Unit] = copy(schema = schema.mapK[F, G](fK))

    given [F[_]] => Contravariant[Constant.Write[F, *]]:
      override def contramap[A, B](fa: Constant.Write[F, A])(f: B => A): Constant.Write[F, B] = fa.contramap(f)

    given [F[_]] => ConstantOperation.Write[Constant.Write[F, *], F]:
      override def lift[A](schema: Reference[F, A], value: Eval[A]): Constant.Write[F, Unit] = Root(schema, value)

      extension [A](fa: Constant.Write[F, A])
        override def encode[T](encoder: Encoder[F, T]): T = fa.encode(encoder)

        override def schema: Reference[F, ?] = fa.schema

  final case class Modify[F[_], A, B](self: Constant[F, A], f: A => B, g: B => A)
      extends Constant.Read[F, B],
        Constant.Write[F, B]:
    export self.{encode, schema}

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Constant[G, B] = copy(self = self.mapK(fK))

  final case class Root[F[_], A](schema: Reference[F, A], value: Eval[A], eq: Eq[A])
      extends Constant.Read[F, Unit],
        Constant.Write[F, Unit]:
    override def encode[T](encoder: Encoder[F, T]): T = encoder.encode(schema.value, value.value)

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Constant[G, Unit] = copy(schema = schema.mapK[F, G](fK))

  given [F[_]] => Invariant[Constant[F, *]]:
    override def imap[A, B](self: Constant[F, A])(f: A => B)(g: B => A): Constant[F, B] = Modify(self, f, g)

  given [F[_]] => ConstantOperation[Constant[F, *], F]:
    override def lift[A](schema: Reference[F, A], value: Eval[A], eq: Eq[A]): Constant[F, Unit] =
      Root(schema, value, eq)

    extension [A](fa: Constant[F, A])
      override def encode[T](encoder: Encoder[F, T]): T = fa.encode(encoder)

      override def schema: Reference[F, ?] = fa.schema
