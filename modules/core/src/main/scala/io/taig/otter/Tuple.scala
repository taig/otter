package io.taig.otter

import cats.Invariant
import cats.data.Chain
import io.taig.otter.operation.TupleOperation

sealed abstract class Tuple[+S[_], A] extends Product with Serializable:
  def schemas: Chain[Reference[S, ?]]

  final def size: Int = schemas.length.toInt

  final def imap[T](f: A => T)(g: T => A): Tuple[S, T] = Tuple.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, A]

object Tuple:
  final case class Default[S[_], A](self: Tuple[S, A], default: A) extends Tuple[S, A]:
    export self.schemas
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, A] =
      copy(self = self.mapK[S1, T](fK))

  case object Empty extends Tuple[Nothing, Unit]:
    override def schemas: Chain[Reference[Nothing, ?]] = Chain.empty

    override def mapK[S1[a] >: Nothing, T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, Unit] = this

  final case class Modify[S[_], A, B](self: Tuple[S, A], f: A => B, g: B => A) extends Tuple[S, B]:
    export self.schemas

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Optional[S[_], A](self: Tuple[S, A]) extends Tuple[S, Option[A]]:
    export self.schemas

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, Option[A]] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](schema: Reference[S, A]) extends Tuple[S, A]:
    override def schemas: Chain[Reference[S, ?]] = Chain.one(schema)

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  final case class Zip[S[_], T[_], A, B](left: Tuple[S, A], right: Tuple[T, B])
      extends Tuple[[a] =>> S[a] | T[a], (A, B)]:
    override def schemas: Chain[Reference[[a] =>> S[a] | T[a], ?]] = left.schemas ++ right.schemas
    override def mapK[S1[a] >: (S[a] | T[a]), U[_]](fK: [A] => S1[A] => U[A]): Tuple[U, (A, B)] =
      copy(left = left.mapK[S1, U](fK), right = right.mapK[S1, U](fK))

  given invariant[S[_]]: Invariant[Tuple[S, *]] with
    override def imap[A, B](fa: Tuple[S, A])(f: A => B)(g: B => A): Tuple[S, B] = fa.imap(f)(g)

  given operation[S[_]]: TupleOperation[Tuple[S, *], S] with
    override def empty: Tuple[S, Unit] = Empty

    override def lift[A](value: => S[A]): Tuple[S, A] = Root(schema = Reference.later(value))

    override def zip[A, B](left: Tuple[S, A], right: Tuple[S, B]): Tuple[S, (A, B)] = Zip(left, right)
