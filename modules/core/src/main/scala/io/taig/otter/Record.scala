package io.taig.otter

import cats.data.Chain
import cats.~>
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.RecordSchemaInvariant

final case class Record[+S[_], A](value: Record.Value[S, A], metadata: Metadata)

object Record:
  sealed abstract class Value[+S[_], A] extends Product, Serializable:
    def fields: Chain[Reference[S, ?]]

    def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Value[T, A]

    final def imap[B](f: A => B)(g: B => A): Value[S, B] = Value.Modify(self = this, f, g)

    final def zip[S1[a] >: S[a], B](schema: Value[S1, B]): Value[S1, (A, B)] =
      Value.Zip(left = this, right = schema)

  object Value:
    private[otter] case object Empty extends Value[Nothing, Unit]:
      override def fields: Chain[Nothing] = Chain.empty
      override def mapK[S1[_] >: Nothing, T[_]](fK: S1 ~> T): Value[T, Unit] = this

    final private[otter] case class Modify[S[_], A, B](self: Value[S, A], f: A => B, g: B => A) extends Value[S, B]:
      export self.fields
      override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Value[T, B] =
        copy(self = self.mapK[S1, T](fK))

    final private[otter] case class Root[S[_], A](field: Reference[S, A]) extends Value[S, A]:
      override def fields: Chain[Reference[S, A]] = Chain.one(field)
      override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Value[T, A] =
        copy(field = field.mapK[S1, T](fK))

    final private[otter] case class Zip[S[_], A, B](left: Value[S, A], right: Value[S, B]) extends Value[S, (A, B)]:
      override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields
      override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Value[T, (A, B)] =
        copy(left = left.mapK[S1, T](fK), right = right.mapK[S1, T](fK))

  given [Field[_]]: RecordSchemaInvariant[Record[Field, *], Field] with
    override def lift[A](field: => Field[A]): Record[Field, A] =
      Record(value = Value.Root(field = Reference.later(field)), metadata = Metadata.Empty)

    override def imap[A, B](fa: Record[Field, A])(f: A => B)(g: B => A): Record[Field, B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Record[Field, A]] = new Enriched[Record[Field, A]]:
      override def metadata(a: Record[Field, A]): Metadata = a.metadata
      override def modifyMetadata(a: Record[Field, A])(f: Metadata => Metadata): Record[Field, A] =
        a.copy(metadata = f(a.metadata))

    extension [A](self: Record[Field, A])
      override def zip[B](schema: Record[Field, B]): Record[Field, (A, B)] =
        Record(value = self.value.zip(schema.value), metadata = self.metadata)
