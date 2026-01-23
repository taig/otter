package io.taig.otter

import cats.Apply
import cats.ContravariantSemigroupal
import cats.Eval
import cats.InvariantSemigroupal
import cats.data.Chain
import io.taig.otter.operation.TupleOperation

type Tuple[+F[_], A] = Tuple.Read[F, A] & Tuple.Write[F, A]

object Tuple:
  sealed trait Read[+F[_], +A]:
    final def map[B](f: A => B): Tuple.Read[F, B] = Read.Modify(self = this, f)

    def optional: Tuple.Read[F, Option[A]] = Read.Optional(self = this)

    def optional[A1 >: A](default: Eval[A1]): Tuple.Read[F, A1] = Read.Default(self = this, value = default)

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple.Read[G, A]

    final def product[F1[a] >: F[a], B](schema: Tuple.Read[F1, B]): Tuple.Read[F1, (A, B)] =
      Read.Product(left = this, right = schema)

    def schemas: Chain[Reference[F, ?]]

  object Read:
    final case class Default[F[_], A](self: Tuple.Read[F, A], value: Eval[A]) extends Tuple.Read[F, A]:
      export self.schemas

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple.Read[G, A] = copy(self = self.mapK(fK))

    final case class Modify[F[_], A, B](self: Tuple.Read[F, A], f: A => B) extends Tuple.Read[F, B]:
      export self.schemas

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple.Read[G, B] = copy(self = self.mapK(fK))

    final case class Optional[F[_], A](self: Tuple.Read[F, A]) extends Tuple.Read[F, Option[A]]:
      export self.schemas

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple.Read[G, Option[A]] = copy(self = self.mapK(fK))

    final case class Product[F[_], A, B](left: Tuple.Read[F, A], right: Tuple.Read[F, B]) extends Tuple.Read[F, (A, B)]:
      override def schemas: Chain[Reference[F, ?]] = left.schemas ++ right.schemas

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple.Read[G, (A, B)] =
        copy(left = left.mapK(fK), right = right.mapK(fK))

    given [F[_]] => Apply[Tuple.Read[F, *]]:
      override def map[A, B](fa: Tuple.Read[F, A])(f: A => B): Tuple.Read[F, B] = fa.map(f)

      override def ap[A, B](ff: Tuple.Read[F, A => B])(fa: Tuple.Read[F, A]): Tuple.Read[F, B] =
        ff.product(fa).map(_ apply _)

    given [F[_]] => TupleOperation.Read[Tuple.Read[F, *], F]:
      override def empty: Tuple.Read[Nothing, Unit] = Empty

      override def lift[A](schema: Reference[F, A]): Tuple.Read[F, A] = Root(schema)

      extension [A](self: Tuple.Read[F, A])
        override def optional: Tuple.Read[F, Option[A]] = self.optional

        override def schemas: Chain[Reference[F, ?]] = self.schemas

  sealed trait Write[+F[_], -A]:
    final def contramap[B](f: B => A): Tuple.Write[F, B] = Write.Modify(self = this, f)

    def optional: Tuple.Write[F, Option[A]] = Tuple.Write.Optional(self = this)

    def schemas: Chain[Reference[F, ?]]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple.Write[G, A]

    final def product[F1[a] >: F[a], B](schema: Tuple.Write[F1, B]): Tuple.Write[F1, (A, B)] =
      Write.Product(left = this, right = schema)

  object Write:
    final case class Modify[F[_], A, B](self: Tuple.Write[F, A], f: B => A) extends Tuple.Write[F, B]:
      export self.schemas

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple.Write[G, B] = copy(self = self.mapK(fK))

    final case class Optional[F[_], A](self: Tuple.Write[F, A]) extends Tuple.Write[F, Option[A]]:
      export self.schemas

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple.Write[G, Option[A]] = copy(self = self.mapK(fK))

    final case class Product[F[_], A, B](left: Tuple.Write[F, A], right: Tuple.Write[F, B])
        extends Tuple.Write[F, (A, B)]:
      override def schemas: Chain[Reference[F, ?]] = left.schemas ++ right.schemas

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple.Write[G, (A, B)] =
        copy(left = left.mapK(fK), right = right.mapK(fK))

    given [F[_]] => ContravariantSemigroupal[Tuple.Write[F, *]]:
      override def contramap[A, B](fa: Tuple.Write[F, A])(f: B => A): Tuple.Write[F, B] =
        fa.contramap(f)

      override def product[A, B](fa: Tuple.Write[F, A], fb: Tuple.Write[F, B]): Tuple.Write[F, (A, B)] = fa.product(fb)

    given [F[_]] => TupleOperation.Write[Tuple.Write[F, *], F]:
      override def empty: Tuple.Write[Nothing, Unit] = Empty

      override def lift[A](schema: Reference[F, A]): Tuple.Write[F, A] = Root(schema)

      extension [A](self: Tuple.Write[F, A])
        override def optional: Tuple.Write[F, Option[A]] = self.optional

        override def schemas: Chain[Reference[F, ?]] = self.schemas

  final case class Default[F[_], A](self: Tuple[F, A], value: Eval[A]) extends Tuple.Read[F, A], Tuple.Write[F, A]:
    export self.schemas

    override def optional: Tuple[F, Option[A]] = Optional(self = this)

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple[G, A] = copy(self = self.mapK(fK))

  case object Empty extends Tuple.Read[Nothing, Unit], Tuple.Write[Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty

    override def optional: Tuple[Nothing, Option[Unit]] = Optional(self = this)

    override def mapK[G[_]](fK: [A] => Nothing => G[A]): Tuple[G, Unit] = this

  final case class Modify[F[_], A, B](self: Tuple[F, A], f: A => B, g: B => A)
      extends Tuple.Read[F, B],
        Tuple.Write[F, B]:
    export self.schemas

    override def optional: Tuple[F, Option[B]] = Optional(self = this)

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple[G, B] = copy(self = self.mapK(fK))

  final case class Optional[F[_], A](self: Tuple[F, A]) extends Tuple.Read[F, Option[A]], Tuple.Write[F, Option[A]]:
    export self.schemas

    override def optional: Tuple[F, Option[Option[A]]] = Optional(self = this)

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple[G, Option[A]] = copy(self = self.mapK(fK))

  final case class Product[F[_], A, B](left: Tuple[F, A], right: Tuple[F, B])
      extends Tuple.Read[F, (A, B)],
        Tuple.Write[F, (A, B)]:
    override def schemas: Chain[Reference[F, ?]] = left.schemas ++ right.schemas

    override def optional: Tuple[F, Option[(A, B)]] = Optional(self = this)

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple[G, (A, B)] =
      copy(left = left.mapK(fK), right = right.mapK(fK))

  final case class Root[F[_], A](schema: Reference[F, A]) extends Tuple.Read[F, A], Tuple.Write[F, A]:
    override def schemas: Chain[Reference[F, A]] = Chain.one(schema)

    override def optional: Tuple[F, Option[A]] = Optional(self = this)

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Tuple[G, A] = copy(schema = schema.mapK[F, G](fK))

  given [F[_]] => InvariantSemigroupal[Tuple[F, *]]:
    override def imap[A, B](self: Tuple[F, A])(f: A => B)(g: B => A): Tuple[F, B] = Modify(self, f, g)

    override def product[A, B](fa: Tuple[F, A], fb: Tuple[F, B]): Tuple[F, (A, B)] = Product(fa, fb)

  given [F[_]] => TupleOperation[Tuple[F, *], F]:
    override def empty: Tuple[F, Unit] = Empty

    override def lift[A](schema: Reference[F, A]): Tuple[F, A] = Root(schema)

    extension [A](self: Tuple[F, A])
      override def optional: Tuple[F, Option[A]] = Optional(self)

      override def schemas: Chain[Reference[F, ?]] = self.schemas
