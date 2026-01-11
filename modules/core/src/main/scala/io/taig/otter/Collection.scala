package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.validation.Validation

sealed abstract class Collection[+S[_], A] extends Collection.Read[S, A], Collection.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): Collection[S, B] = Collection.Modify(self = this, f, g)

object Collection:
  sealed trait Read[+S[_], +A]:
    def schema: Reference[S, ?]

    final def map[B](f: A => B): Collection.Read[S, B] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[S[_], A, B](self: Collection.Read[S, A], f: A => B) extends Collection.Read[S, B]:
      export self.schema

    given [S[_]] => Functor[Collection.Read[S, *]]:
      override def map[A, B](fa: Collection.Read[S, A])(f: A => B): Collection.Read[S, B] = fa.map(f)

  sealed trait Write[+S[_], -A]:
    def schema: Reference[S, ?]

    final def contramap[B](f: B => A): Collection.Write[S, B] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[S[_], A, B](self: Collection.Write[S, A], f: B => A) extends Collection.Write[S, B]:
      export self.schema

    given [S[_]] => Contravariant[Collection.Write[S, *]]:
      override def contramap[A, B](fa: Collection.Write[S, A])(f: B => A): Collection.Write[S, B] =
        fa.contramap(f)

  final case class Chained[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Chain[A]])
      extends Collection[S, Chain[A]]

  final case class Indexed[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Vector[A]])
      extends Collection[S, Vector[A]]

  final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, List[A]])
      extends Collection[S, List[A]]
  final case class Modify[S[_], A, B](self: Collection[S, A], f: A => B, g: B => A) extends Collection[S, B]:
    export self.schema

  given [S[_]] => Invariant[Collection[S, *]]:
    override def imap[A, B](self: Collection[S, A])(f: A => B)(g: B => A): Collection[S, B] = self.imap(f)(g)
