package io.taig.otter

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.UnionSchemaInvariant

final case class Union[+S[_], A](value: Union.Value[S, A], metadata: Metadata)

object Union:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def schemas: NonEmptyChain[Reference[S, ?]]

    final def imap[B](f: A => B)(g: B => A): Value[S, B] = Value.Modify(self = this, f, g)

    final def orElse[S1[a] >: S[a], B](schema: Value[S1, B]): Value[S1, Either[A, B]] =
      Value.OrElse(left = this, right = schema)

    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, A]

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Value[S, A], f: A => B, g: B => A) extends Value[S, B]:
      export self.schemas
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, B] = copy(self = self.mapK[S1, T](fK))

    final private[otter] case class OrElse[S[_], A, B](left: Value[S, A], right: Value[S, B])
        extends Value[S, Either[A, B]]:
      override def schemas: NonEmptyChain[Reference[S, ?]] = left.schemas ++ right.schemas
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, Either[A, B]] =
        copy(left = left.mapK[S1, T](fK), right = right.mapK[S1, T](fK))

    final private[otter] case class Root[S[_], A](schema: Reference[S, A]) extends Value[S, A]:
      override def schemas: NonEmptyChain[Reference[S, ?]] = NonEmptyChain.one(schema)
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, A] =
        copy(schema = schema.mapK[S1, T](fK))

  given [Value[_]]: UnionSchemaInvariant[Union[Value, *], Value] with
    override def lift[A](schema: => Value[A]): Union[Value, A] =
      Union(value = Value.Root(schema = Reference.later(schema)), metadata = Metadata.Empty)

    override def imap[A, B](fa: Union[Value, A])(f: A => B)(g: B => A): Union[Value, B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Union[Value, A]] = new Enriched[Union[Value, A]]:
      override def metadata(a: Union[Value, A]): Metadata = a.metadata
      override def modifyMetadata(a: Union[Value, A])(f: Metadata => Metadata): Union[Value, A] =
        a.copy(metadata = f(a.metadata))

    extension [A](self: Union[Value, A])
      override def schemas: NonEmptyChain[Reference[Value, ?]] = self.value.schemas
      override def orElse[B](schema: Union[Value, B]): Union[Value, Either[A, B]] =
        self.copy(value = self.value.orElse(schema.value))
