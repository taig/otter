package io.taig.otter
import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.EnumerationSchemaInvariant

final case class Enumeration[+S[_], A](value: Enumeration.Value[S, A], metadata: Metadata)

object Enumeration:
  sealed abstract class Value[+S[_], A] extends Product, Serializable:
    def schema: Reference[S, ?]

    def values: NonEmptyList[A]

    final def imap[B](f: A => B)(g: B => A): Value[S, B] = Value.Modify(self = this, f, g)
    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, A]

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Value[S, A], f: A => B, g: B => A) extends Value[S, B]:
      export self.schema
      override def values: NonEmptyList[B] = self.values.map(f)
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, B] =
        copy(self = self.mapK[S1, T](fK))

    final private[otter] case class Root[S[_], A, B](schema: Reference[S, A], mapping: Mapping[B, A])
        extends Value[S, B]:
      override def values: NonEmptyList[B] = mapping.values
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, B] =
        copy(schema = schema.mapK[S1, T](fK))

  given [Value[_]]: EnumerationSchemaInvariant[Enumeration[Value, *], Value] with
    override def apply[A, B](schema: => Value[A], mapping: Mapping[B, A]): Enumeration[Value, B] =
      Enumeration(
        value = Value.Root(schema = Reference.later(schema), mapping = mapping),
        metadata = Metadata.Empty
      )

    override def imap[A, B](fa: Enumeration[Value, A])(f: A => B)(g: B => A): Enumeration[Value, B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Enumeration[Value, A]] = new Enriched[Enumeration[Value, A]]:
      override def metadata(a: Enumeration[Value, A]): Metadata = a.metadata
      override def modifyMetadata(a: Enumeration[Value, A])(f: Metadata => Metadata): Enumeration[Value, A] =
        a.copy(metadata = f(a.metadata))

    extension [A](self: Enumeration[Value, A])
      override def schema: Reference[Value, ?] = self.value.schema
      override def values: NonEmptyList[A] = self.value.values
