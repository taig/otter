package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.Reference
import cats.Eq
import cats.Eval
import io.taig.otter.operation.ConstantOperation

sealed abstract class Constant[+F[_], A] extends Constant.Read[F, A], Constant.Write[F, A]:
  final def imap[B](f: A => B)(g: B => A): Constant[F, B] = Constant.Modify(self = this, f, g)

object Constant:
  sealed trait Read[+F[_], +A]:
    def schema: Reference[F, ?]

    final def map[B](f: A => B): Constant.Read[F, B] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[F[_], A, B](self: Constant.Read[F, A], f: A => B) extends Constant.Read[F, B]:
      export self.schema

    given [F[_]] => Functor[Constant.Read[F, *]]:
      override def map[A, B](fa: Constant.Read[F, A])(f: A => B): Constant.Read[F, B] = fa.map(f)

    given [F[_]] => ConstantOperation.Read[Constant.Read[F, *], F]:
      override def lift[A](schema: Reference[F, A], value: Eval[A], eq: Eq[A]): Constant.Read[F, A] =
        Root(schema, value, eq)

      extension [A](fa: Constant.Read[F, A]) override def schema: Reference[F, ?] = fa.schema

  sealed trait Write[+F[_], -A]:
    def schema: Reference[F, ?]

    final def contramap[B](f: B => A): Constant.Write[F, B] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[F[_], A, B](self: Constant.Write[F, A], f: B => A) extends Constant.Write[F, B]:
      export self.schema

    final case class Root[F[_], A](schema: Reference[F, A], value: Eval[A]) extends Constant.Write[F, A]

    given [F[_]] => Contravariant[Constant.Write[F, *]]:
      override def contramap[A, B](fa: Constant.Write[F, A])(f: B => A): Constant.Write[F, B] = fa.contramap(f)

    given [F[_]] => ConstantOperation.Write[Constant.Write[F, *], F]:
      override def lift[A](schema: Reference[F, A], value: Eval[A]): Constant.Write[F, A] = Root(schema, value)

      extension [A](fa: Constant.Write[F, A]) override def schema: Reference[F, ?] = fa.schema

  final case class Modify[F[_], A, B](self: Constant[F, A], f: A => B, g: B => A) extends Constant[F, B]:
    export self.schema

  final case class Root[F[_], A](schema: Reference[F, A], value: Eval[A], eq: Eq[A]) extends Constant[F, A]

  given [F[_]] => Invariant[Constant[F, *]]:
    override def imap[A, B](self: Constant[F, A])(f: A => B)(g: B => A): Constant[F, B] = self.imap(f)(g)

  given [F[_]] => ConstantOperation[Constant[F, *], F]:
    override def lift[A](schema: Reference[F, A], value: Eval[A], eq: Eq[A]): Constant[F, A] =
      Root(schema, value, eq)

    extension [A](fa: Constant[F, A]) override def schema: Reference[F, ?] = fa.schema
