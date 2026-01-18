package io.taig.otter

import cats.Contravariant
import cats.Eval
import cats.Functor
import cats.Invariant
import io.taig.otter.operation.OptionalOperation

sealed abstract class Optional[+F[_], A] extends Optional.Read[F, A], Optional.Write[F, A]:
  final def imap[B](f: A => B)(g: B => A): Optional[F, B] = Optional.Modify(self = this, f, g)

  def mapK[G[_]](fK: [A] => F[A] => G[A]): Optional[G, A]

object Optional:
  sealed trait Read[+F[_], +A]:
    def schema: Reference[F, ?]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Optional.Read[G, A]

    final def map[B](f: A => B): Optional.Read[F, B] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[F[_], A, B](self: Optional.Read[F, A], f: A => B) extends Optional.Read[F, B]:
      export self.schema

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Optional.Read[G, B] = copy(self = self.mapK(fK))

    given [F[_]] => Functor[Optional.Read[F, *]]:
      override def map[A, B](fa: Optional.Read[F, A])(f: A => B): Optional.Read[F, B] = fa.map(f)

    given [F[_]] => OptionalOperation.Read[Optional.Read[F, *], F]:
      override def lift[A](schema: => Reference[F, A]): Optional.Read[F, Option[A]] = Root(schema)

      override def lift[A](schema: => Reference[F, A], default: => A): Optional.Read[F, A] =
        Default(schema, default = Eval.later(default))

      extension [A](fa: Read[F, A]) override def schema: Reference[F, ?] = fa.schema

  sealed trait Write[+F[_], -A]:
    def schema: Reference[F, ?]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Optional.Write[G, A]

    final def contramap[B](f: B => A): Optional.Write[F, B] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[F[_], A, B](self: Optional.Write[F, A], f: B => A) extends Optional.Write[F, B]:
      export self.schema

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Optional.Write[G, B] = copy(self = self.mapK(fK))

    given [F[_]] => Contravariant[Optional.Write[F, *]]:
      override def contramap[A, B](fa: Optional.Write[F, A])(f: B => A): Optional.Write[F, B] = fa.contramap(f)

    given [F[_]] => OptionalOperation.Write[Optional.Write[F, *], F]:
      override def lift[A](schema: => Reference[F, A]): Optional.Write[F, Option[A]] = Root(schema)

      override def lift[A](schema: => Reference[F, A], default: => A): Optional.Write[F, A] =
        Default(schema, default = Eval.later(default))
      extension [A](fa: Optional.Write[F, A]) override def schema: Reference[F, ?] = fa.schema

  final case class Default[F[_], A](schema: Reference[F, A], default: Eval[A]) extends Optional[F, A]:
    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Optional[G, A] = copy(schema = schema.mapK[F, G](fK))

  final case class Modify[F[_], A, B](self: Optional[F, A], f: A => B, g: B => A) extends Optional[F, B]:
    export self.schema

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Optional[G, B] = copy(self = self.mapK(fK))

  final case class Root[F[_], A](schema: Reference[F, A]) extends Optional[F, Option[A]]:
    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Optional[G, Option[A]] = copy(schema = schema.mapK[F, G](fK))

  given [F[_]] => Invariant[Optional[F, *]]:
    override def imap[A, B](self: Optional[F, A])(f: A => B)(g: B => A): Optional[F, B] = self.imap(f)(g)

  given [F[_]] => OptionalOperation[Optional[F, *], F]:
    override def lift[A](schema: => Reference[F, A]): Optional[F, Option[A]] = Root(schema)

    override def lift[A](schema: => Reference[F, A], default: => A): Optional[F, A] =
      Default(schema, default = Eval.later(default))

    extension [A](fa: Optional[F, A]) override def schema: Reference[F, ?] = fa.schema
