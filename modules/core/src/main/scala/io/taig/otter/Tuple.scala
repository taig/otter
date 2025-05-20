package io.taig.otter

import cats.data.Chain
import io.taig.otter.schema.TupleSchema

// TODO support for optional
sealed abstract class Tuple[+S[_], A] extends Product with Serializable:
  def schemas: Chain[Reference[S, ?]]

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, A]

  final def imap[B](f: A => B)(g: B => A): Tuple[S, B] = Tuple.Modify(self = this, f, g)

  final def zip[S1[a] >: S[a], B](schema: Tuple[S1, B]): Tuple[S1, (A, B)] =
    Tuple.Zip(left = this, right = schema)

object Tuple:
  private[otter] case object Empty extends Tuple[Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty
    override def mapK[S1[a] >: Nothing, T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, Unit] = this

  final private[otter] case class Modify[S[_], A, B](self: Tuple[S, A], f: A => B, g: B => A) extends Tuple[S, B]:
    export self.schemas
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, B] = copy(self = self.mapK[S1, T](fK))

  final private[otter] case class Root[S[_], A](schema: Reference[S, A]) extends Tuple[S, A]:
    override def schemas: Chain[Reference[S, A]] = Chain(schema)
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  final private[otter] case class Zip[S[_], A, B](
      left: Tuple[S, A],
      right: Tuple[S, B]
  ) extends Tuple[S, (A, B)]:
    override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Tuple[T, (A, B)] =
      copy(left = left.mapK[S1, T](fK), right = right.mapK[S1, T](fK))

  given [Value[_]]: TupleSchema[Tuple[Value, *], Value] with
    override def empty: Tuple[Value, Unit] = Tuple.Empty

    override def lift[A](schema: => Value[A]): Tuple[Value, A] =
      Tuple.Root(schema = Reference.later(schema))

    override def imap[A, B](fa: Tuple[Value, A])(f: A => B)(g: B => A): Tuple[Value, B] = fa.imap(f)(g)

    extension [A](self: Tuple[Value, A])
      override def zip[B](schema: Tuple[Value, B]): Tuple[Value, (A, B)] = self.zip(schema)
