package io.taig.otter

import cats.data.Chain
import io.taig.otter.Metadata
import io.taig.otter.schema.RecordSchema

sealed abstract class Record[+S[_], A] extends Product with Serializable:
  def fields: Chain[Reference[S, ?]]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Record[S, A]

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, A]

  final def imap[B](f: A => B)(g: B => A): Record[S, B] = Record.Modify(self = this, f, g)

  final def optional: Record[S, Option[A]] = Record.Optional(self = this)

  final def zip[S1[a] >: S[a], B](schema: Record[S1, B]): Record[S1, (A, B)] =
    Record.Zip(left = this, right = schema, metadata = Metadata.Empty)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Unit] = copy(metadata = f(metadata))
    override def mapK[S1[_] >: Nothing, T[_]](fK: [A] => S1[A] => T[A]): Record[T, Unit] = this

  final private[otter] case class Modify[S[_], A, B](self: Record[S, A], f: A => B, g: B => A) extends Record[S, B]:
    export self.{fields, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Record[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final private[otter] case class Optional[S[_], A](self: Record[S, A]) extends Record[S, Option[A]]:
    export self.{fields, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Record[S, Option[A]] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, Option[A]] =
      copy(self = self.mapK[S1, T](fK))

  final private[otter] case class Root[S[_], A](field: Reference[S, A], metadata: Metadata) extends Record[S, A]:
    override def fields: Chain[Reference[S, A]] = Chain.one(field)
    override def modifyMetadata(f: Metadata => Metadata): Record[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, A] =
      copy(field = field.mapK[S1, T](fK))

  final private[otter] case class Zip[S[_], A, B](
      left: Record[S, A],
      right: Record[S, B],
      metadata: Metadata
  ) extends Record[S, (A, B)]:
    override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields
    override def modifyMetadata(f: Metadata => Metadata): Record[S, (A, B)] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, (A, B)] =
      copy(left = left.mapK[S1, T](fK), right = right.mapK[S1, T](fK))

  given [Field[_]]: RecordSchema[Record[Field, *], Field] with
    override def lift[A](field: => Field[A]): Record[Field, A] =
      Root(field = Reference.later(field), metadata = Metadata.Empty)

    override def imap[A, B](fa: Record[Field, A])(f: A => B)(g: B => A): Record[Field, B] = fa.imap(f)(g)

    extension [A](self: Record[Field, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Record[Field, A] = self.modifyMetadata(f)
      override def zip[B](schema: Record[Field, B]): Record[Field, (A, B)] = self.zip(schema)
      override def optional: Record[Field, Option[A]] = self.optional
