package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.NonEmptyChain
import io.taig.otter.operation.UnionOperation

sealed abstract class Union[+F[_], A] extends Union.Read[F, A], Union.Write[F, A]:
  final def imap[B](f: A => B)(g: B => A): Union[F, B] = Union.Modify(self = this, f, g)

  def mapK[G[_]](fK: [A] => F[A] => G[A]): Union[G, A]

  final def coproduct[F1[a] >: F[a], B](union: Union[F1, B]): Union[F1, Either[A, B]] =
    Union.Coproduct(left = this, right = union)

object Union:
  sealed trait Read[+F[_], +A]:
    def branches: NonEmptyChain[Reference[F, ?]]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Union.Read[G, A]

    final def map[B](f: A => B): Union.Read[F, B] = Read.Modify(self = this, f)

    final def coproduct[F1[a] >: F[a], B](union: Union.Read[F1, B]): Union.Read[F1, Either[A, B]] =
      Read.Coproduct(left = this, right = union)

  object Read:
    final case class Modify[F[_], A, B](self: Union.Read[F, A], f: A => B) extends Union.Read[F, B]:
      export self.branches

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Union.Read[G, B] = copy(self = self.mapK(fK))

    final case class Coproduct[F[_], A, B](left: Union.Read[F, A], right: Union.Read[F, B])
        extends Union.Read[F, Either[A, B]]:
      override def branches: NonEmptyChain[Reference[F, ?]] = left.branches ++ right.branches

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Union.Read[G, Either[A, B]] =
        copy(left = left.mapK(fK), right = right.mapK(fK))

    given [F[_]] => Functor[Union.Read[F, *]]:
      override def map[A, B](fa: Union.Read[F, A])(f: A => B): Union.Read[F, B] = fa.map(f)

    given [F[_]] => UnionOperation.Read[Union.Read[F, *], F]:
      override def lift[A](branch: Reference[F, A]): Union.Read[F, A] = Root(branch)

      extension [A](fa: Union.Read[F, A]) override def branches: NonEmptyChain[Reference[F, ?]] = fa.branches

  sealed trait Write[+F[_], -A]:
    def branches: NonEmptyChain[Reference[F, ?]]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Union.Write[G, A]

    final def contramap[B](f: B => A): Union.Write[F, B] = Write.Modify(self = this, f)

    final def coproduct[F1[a] >: F[a], B](union: Union.Write[F1, B]): Union.Write[F1, Either[A, B]] =
      Write.Coproduct(left = this, right = union)

  object Write:
    final case class Modify[F[_], A, B](self: Union.Write[F, A], f: B => A) extends Union.Write[F, B]:
      export self.branches

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Union.Write[G, B] = copy(self = self.mapK(fK))

    final case class Coproduct[F[_], A, B](left: Union.Write[F, A], right: Union.Write[F, B])
        extends Union.Write[F, Either[A, B]]:
      override def branches: NonEmptyChain[Reference[F, ?]] = left.branches ++ right.branches

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Union.Write[G, Either[A, B]] =
        copy(left = left.mapK(fK), right = right.mapK(fK))

    given [F[_]] => Contravariant[Union.Write[F, *]]:
      override def contramap[A, B](fa: Union.Write[F, A])(f: B => A): Union.Write[F, B] = fa.contramap(f)

    given [F[_]] => UnionOperation.Write[Union.Write[F, *], F]:
      override def lift[A](branch: Reference[F, A]): Union.Write[F, A] = Root(branch)

      extension [A](fa: Union.Write[F, A]) override def branches: NonEmptyChain[Reference[F, ?]] = fa.branches

  final case class Modify[F[_], A, B](self: Union[F, A], f: A => B, g: B => A) extends Union[F, B]:
    export self.branches

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Union[G, B] = copy(self = self.mapK(fK))

  final case class Coproduct[F[_], A, B](left: Union[F, A], right: Union[F, B]) extends Union[F, Either[A, B]]:
    override def branches: NonEmptyChain[Reference[F, ?]] = left.branches ++ right.branches

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Union[G, Either[A, B]] =
      copy(left = left.mapK(fK), right = right.mapK(fK))

  final case class Root[F[_], A](branch: Reference[F, A]) extends Union[F, A]:
    override def branches: NonEmptyChain[Reference[F, ?]] = NonEmptyChain.one(branch)

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Union[G, A] = copy(branch = branch.mapK[F, G](fK))

  given [F[_]] => Invariant[Union[F, *]]:
    override def imap[A, B](fa: Union[F, A])(f: A => B)(g: B => A): Union[F, B] = fa.imap(f)(g)

  given [F[_]] => UnionOperation[Union[F, *], F]:
    override def lift[A](branch: Reference[F, A]): Union[F, A] = Root(branch)

    extension [A](fa: Union[F, A]) override def branches: NonEmptyChain[Reference[F, ?]] = fa.branches
