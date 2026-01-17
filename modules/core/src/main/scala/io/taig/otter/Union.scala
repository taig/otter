package io.taig.otter

import cats.Functor
import cats.Contravariant
import cats.Invariant
import io.taig.otter.operation.UnionOperation
import cats.data.NonEmptyChain

sealed abstract class Union[+F[_], A] extends Union.Read[F, A], Union.Write[F, A]:
  final def imap[B](f: A => B)(g: B => A): Union[F, B] = Union.Modify(self = this, f, g)

  final def coproduct[F1[a] >: F[a], B](union: Union[F1, B]): Union[F1, Either[A, B]] =
    Union.Coproduct(left = this, right = union)

object Union:
  sealed trait Read[+F[_], +A]:
    def branches: NonEmptyChain[Reference[F, ?]]

    final def map[B](f: A => B): Union.Read[F, B] = Read.Modify(self = this, f)

    final def coproduct[F1[a] >: F[a], B](union: Union.Read[F1, B]): Union.Read[F1, Either[A, B]] =
      Read.Coproduct(left = this, right = union)

  object Read:
    final case class Modify[F[_], A, B](self: Union.Read[F, A], f: A => B) extends Union.Read[F, B]:
      export self.branches

    final case class Coproduct[F[_], A, B](left: Union.Read[F, A], right: Union.Read[F, B])
        extends Union.Read[F, Either[A, B]]:
      override def branches: NonEmptyChain[Reference[F, ?]] = left.branches ++ right.branches

    given [F[_]] => Functor[Union.Read[F, *]]:
      override def map[A, B](fa: Union.Read[F, A])(f: A => B): Union.Read[F, B] = fa.map(f)

    given [F[_]] => UnionOperation.Read[Union.Read[F, *], F]:
      override def lift[A](branch: Reference[F, A]): Union.Read[F, A] = Root(branch)

      extension [A](fa: Union.Read[F, A]) override def branches: NonEmptyChain[Reference[F, ?]] = fa.branches

  sealed trait Write[+F[_], -A]:
    def branches: NonEmptyChain[Reference[F, ?]]

    final def contramap[B](f: B => A): Union.Write[F, B] = Write.Modify(self = this, f)

    final def coproduct[F1[a] >: F[a], B](union: Union.Write[F1, B]): Union.Write[F1, Either[A, B]] =
      Write.Coproduct(left = this, right = union)

  object Write:
    final case class Modify[F[_], A, B](self: Union.Write[F, A], f: B => A) extends Union.Write[F, B]:
      export self.branches

    final case class Coproduct[F[_], A, B](left: Union.Write[F, A], right: Union.Write[F, B])
        extends Union.Write[F, Either[A, B]]:
      override def branches: NonEmptyChain[Reference[F, ?]] = left.branches ++ right.branches

    given [F[_]] => Contravariant[Union.Write[F, *]]:
      override def contramap[A, B](fa: Union.Write[F, A])(f: B => A): Union.Write[F, B] = fa.contramap(f)

    given [F[_]] => UnionOperation.Write[Union.Write[F, *], F]:
      override def lift[A](branch: Reference[F, A]): Union.Write[F, A] = Root(branch)

      extension [A](fa: Union.Write[F, A]) override def branches: NonEmptyChain[Reference[F, ?]] = fa.branches

  final case class Modify[F[_], A, B](self: Union[F, A], f: A => B, g: B => A) extends Union[F, B]:
    export self.branches

  final case class Coproduct[F[_], A, B](left: Union[F, A], right: Union[F, B]) extends Union[F, Either[A, B]]:
    override def branches: NonEmptyChain[Reference[F, ?]] = left.branches ++ right.branches

  final case class Root[F[_], A](branch: Reference[F, A]) extends Union[F, A]:
    override def branches: NonEmptyChain[Reference[F, ?]] = NonEmptyChain.one(branch)

  given [F[_]] => Invariant[Union[F, *]]:
    override def imap[A, B](fa: Union[F, A])(f: A => B)(g: B => A): Union[F, B] = fa.imap(f)(g)

  given [F[_]] => UnionOperation[Union[F, *], F]:
    override def lift[A](branch: Reference[F, A]): Union[F, A] = Root(branch)

    extension [A](fa: Union[F, A]) override def branches: NonEmptyChain[Reference[F, ?]] = fa.branches
