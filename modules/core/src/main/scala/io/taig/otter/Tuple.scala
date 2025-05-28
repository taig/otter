package io.taig.otter

import cats.data.Chain
import io.taig.otter.operation.TupleSchemaInvariant

// TODO support for optional
final case class Tuple[+S[_], A](value: Tuple.Value[S, A], metadata: Metadata)

object Tuple:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def schemas: Chain[Reference[S, ?]]

    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, A]

    final def imap[B](f: A => B)(g: B => A): Value[S, B] = Value.Modify(self = this, f, g)

    final def zip[S1[a] >: S[a], B](schema: Value[S1, B]): Value[S1, (A, B)] =
      Value.Zip(left = this, right = schema)

  object Value:
    private[otter] case object Empty extends Value[Nothing, Unit]:
      override def schemas: Chain[Nothing] = Chain.empty
      override def mapK[S1[a] >: Nothing, T[_]](fK: [A] => S1[A] => T[A]): Value[T, Unit] = this

    final private[otter] case class Modify[S[_], A, B](self: Value[S, A], f: A => B, g: B => A) extends Value[S, B]:
      export self.schemas
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, B] = copy(self = self.mapK[S1, T](fK))

    final private[otter] case class Root[S[_], A](schema: Reference[S, A]) extends Value[S, A]:
      override def schemas: Chain[Reference[S, A]] = Chain.one(schema)
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, A] =
        copy(schema = schema.mapK[S1, T](fK))

    final private[otter] case class Zip[S[_], A, B](
        left: Value[S, A],
        right: Value[S, B]
    ) extends Value[S, (A, B)]:
      override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, (A, B)] =
        copy(left = left.mapK[S1, T](fK), right = right.mapK[S1, T](fK))

  given [Value[_]]: TupleSchemaInvariant[Tuple[Value, *], Value] with
    override val empty: Tuple[Value, Unit] =
      Tuple(value = Value.Empty, metadata = Metadata.Empty)

    override def lift[A](schema: => Value[A]): Tuple[Value, A] =
      Tuple(value = Value.Root(schema = Reference.later(schema)), metadata = Metadata.Empty)

    override def imap[A, B](fa: Tuple[Value, A])(f: A => B)(g: B => A): Tuple[Value, B] =
      fa.copy(value = fa.value.imap(f)(g))

    extension [A](self: Tuple[Value, A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Tuple[Value, A] =
        self.copy(metadata = f(self.metadata))
      override def zip[B](schema: Tuple[Value, B]): Tuple[Value, (A, B)] =
        Tuple(value = self.value.zip(schema.value), metadata = self.metadata)
