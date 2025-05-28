package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.NullableSchemaInvariant

final case class Nullable[+S[_], A](value: Nullable.Value[S, A], metadata: Metadata)

object Nullable:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def schema: Option[Reference[S, ?]]

    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, A]
    final def imap[B](f: A => B)(g: B => A): Value[S, B] = Value.Modify(self = this, f, g)

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Value[S, A], f: A => B, g: B => A) extends Value[S, B]:
      export self.schema
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, B] = copy(self = self.mapK[S1, T](fK))

    final private[otter] case class Default[S[_], A](reference: Reference[S, A], default: A) extends Value[S, A]:
      override def schema: Option[Reference[S, ?]] = reference.some
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, A] =
        copy(reference = reference.mapK[S1, T](fK))

    final private[otter] case class Root[S[_], A](reference: Reference[S, A]) extends Value[S, Option[A]]:
      override def schema: Option[Reference[S, ?]] = reference.some
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, Option[A]] =
        copy(reference = reference.mapK[S1, T](fK))

    private[otter] case object Void extends Value[Nothing, Unit]:
      override def schema: Option[Reference[Nothing, ?]] = none
      override def mapK[S1[a] >: Nothing, T[_]](fK: [A] => S1[A] => T[A]): Value[T, Unit] = this

  given [Value[_]]: NullableSchemaInvariant[Nullable[Value, *], Value] with
    override def apply[A](schema: => Value[A]): Nullable[Value, Option[A]] =
      Nullable(value = Value.Root(reference = Reference.later(schema)), metadata = Metadata.Empty)
    override def apply[A](schema: => Value[A], default: A): Nullable[Value, A] =
      Nullable(value = Value.Default(reference = Reference.later(schema), default = default), metadata = Metadata.Empty)
    override val void: Nullable[Nothing, Unit] =
      Nullable(value = Value.Void, metadata = Metadata.Empty)

    override def imap[A, B](fa: Nullable[Value, A])(f: A => B)(g: B => A): Nullable[Value, B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Nullable[Value, A]] = new Enriched[Nullable[Value, A]]:
      override def metadata(a: Nullable[Value, A]): Metadata = a.metadata
      override def modifyMetadata(a: Nullable[Value, A])(f: Metadata => Metadata): Nullable[Value, A] =
        a.copy(metadata = f(a.metadata))
