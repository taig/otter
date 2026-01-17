package io.taig.otter

import cats.Functor
import cats.Contravariant
import cats.Invariant
import io.taig.otter.operation.UnionOperation
import cats.data.NonEmptyChain

sealed abstract class Union[+S[_], A] extends Union.Read[S, A], Union.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): Union[S, B] = Union.Modify(self = this, f, g)

  final def coproduct[S1[a] >: S[a], B](union: Union[S1, B]): Union[S1, Either[A, B]] =
    Union.Coproduct(left = this, right = union)

object Union:
  sealed trait Read[+S[_], +A]:
    def branches: NonEmptyChain[Reference[S, ?]]

    final def map[B](f: A => B): Union.Read[S, B] = Read.Modify(self = this, f)

    final def coproduct[S1[a] >: S[a], B](union: Union.Read[S1, B]): Union.Read[S1, Either[A, B]] =
      Read.Coproduct(left = this, right = union)

  object Read:
    final case class Modify[S[_], A, B](self: Union.Read[S, A], f: A => B) extends Union.Read[S, B]:
      export self.branches

    final case class Coproduct[S[_], A, B](left: Union.Read[S, A], right: Union.Read[S, B])
        extends Union.Read[S, Either[A, B]]:
      override def branches: NonEmptyChain[Reference[S, ?]] = left.branches ++ right.branches

    given [F[_]] => Functor[Union.Read[F, *]]:
      override def map[A, B](fa: Union.Read[F, A])(f: A => B): Union.Read[F, B] = fa.map(f)

    given [F[_]] => UnionOperation.Read[Union.Read[F, *], F]:
      override def lift[A](branch: Reference[F, A]): Union.Read[F, A] = Root(branch)

      extension [A](fa: Union.Read[F, A]) override def branches: NonEmptyChain[Reference[F, ?]] = fa.branches

  sealed trait Write[+S[_], -A]:
    def branches: NonEmptyChain[Reference[S, ?]]

    final def contramap[B](f: B => A): Union.Write[S, B] = Write.Modify(self = this, f)

    final def coproduct[S1[a] >: S[a], B](union: Union.Write[S1, B]): Union.Write[S1, Either[A, B]] =
      Write.Coproduct(left = this, right = union)

  object Write:
    final case class Modify[S[_], A, B](self: Union.Write[S, A], f: B => A) extends Union.Write[S, B]:
      export self.branches

    final case class Coproduct[S[_], A, B](left: Union.Write[S, A], right: Union.Write[S, B])
        extends Union.Write[S, Either[A, B]]:
      override def branches: NonEmptyChain[Reference[S, ?]] = left.branches ++ right.branches

    given [F[_]] => Contravariant[Union.Write[F, *]]:
      override def contramap[A, B](fa: Union.Write[F, A])(f: B => A): Union.Write[F, B] = fa.contramap(f)

    given [F[_]] => UnionOperation.Write[Union.Write[F, *], F]:
      override def lift[A](branch: Reference[F, A]): Union.Write[F, A] = Root(branch)

      extension [A](fa: Union.Write[F, A]) override def branches: NonEmptyChain[Reference[F, ?]] = fa.branches

  final case class Modify[S[_], A, B](self: Union[S, A], f: A => B, g: B => A) extends Union[S, B]:
    export self.branches

  final case class Coproduct[S[_], A, B](left: Union[S, A], right: Union[S, B]) extends Union[S, Either[A, B]]:
    override def branches: NonEmptyChain[Reference[S, ?]] = left.branches ++ right.branches

  final case class Root[S[_], A](branch: Reference[S, A]) extends Union[S, A]:
    override def branches: NonEmptyChain[Reference[S, ?]] = NonEmptyChain.one(branch)

  given [F[_]] => Invariant[Union[F, *]]:
    override def imap[A, B](fa: Union[F, A])(f: A => B)(g: B => A): Union[F, B] = fa.imap(f)(g)

  given [F[_]] => UnionOperation[Union[F, *], F]:
    override def lift[A](branch: Reference[F, A]): Union[F, A] = Root(branch)

    extension [A](fa: Union[F, A]) override def branches: NonEmptyChain[Reference[F, ?]] = fa.branches
