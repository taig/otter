package io.taig.otter

import cats.Eq
import cats.syntax.all.*
import io.taig.otter.operation.ConstantSchemaInvariant

final case class Constant[+S[_], A](value: Constant.Value[S, A], metadata: Metadata)

object Constant:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def schema: Reference.Constant[S, ?]

    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant.Value[T, A]
    final def imap[B](f: A => B)(g: B => A): Constant.Value[S, B] = Value.Modify(self = this, f, g)

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Constant.Value[S, A], f: A => B, g: B => A)
        extends Constant.Value[S, B]:
      export self.schema
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant.Value[T, B] =
        copy(self = self.mapK[S1, T](fK))

    final private[otter] case class Root[S[_], A](
        schema: Reference.Constant[S, A],
        eq: Eq[A]
    ) extends Constant.Value[S, Unit]:
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant.Value[T, Unit] =
        copy(schema = schema.mapK[S1, T](fK))

  given [Value[_]]: ConstantSchemaInvariant[Constant[Value, *], Value] with
    override def apply[A](schema: => Value[A], value: A)(using eq: Eq[A]): Constant[Value, Unit] = Constant(
      value = Value.Root(schema = Reference.Constant(self = Reference.later(schema), value), eq),
      metadata = Metadata.Empty
    )

    override def imap[A, B](fa: Constant[Value, A])(f: A => B)(g: B => A): Constant[Value, B] =
      fa.copy(value = fa.value.imap(f)(g))

    extension [A](self: Constant[Value, A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Constant[Value, A] = self.copy(metadata = f(self.metadata))
