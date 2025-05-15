package io.taig.otter

import cats.data.Chain
import io.taig.otter.Metadata
import io.taig.otter.schema.RecordSchema

sealed abstract class Record[+S[_], +T[_], A] extends Product with Serializable:
  def fields: Chain[Field[S, T, ?]]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Record[S, T, A]

  def isOptional: Boolean

  def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Record[S, U, A]

  final def imap[B](f: A => B)(g: B => A): Record[S, T, B] = Record.Modify(self = this, f, g)

  final def optional: Record[S, T, Option[A]] = Record.Optional(self = this)

  final def zip[S1[a] >: S[a], T1[a] >: T[a], B](schema: Record[S1, T1, B]): Record[S1, T1, (A, B)] =
    Record.Zip(left = this, right = schema, metadata = Metadata.Empty)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Nothing, Unit]:
    override def isOptional: Boolean = false
    override def fields: Chain[Nothing] = Chain.empty
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Nothing, Unit] = copy(metadata = f(metadata))
    override def mapK[S1[_] >: Nothing, T[_]](fK: [A] => S1[A] => T[A]): Record[Nothing, T, Unit] = this

  final private[otter] case class Modify[S[_], T[_], A, B](self: Record[S, T, A], f: A => B, g: B => A)
      extends Record[S, T, B]:
    export self.{fields, isOptional, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Record[S, U, B] =
      copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Optional[S[_], T[_], A](self: Record[S, T, A]) extends Record[S, T, Option[A]]:
    export self.{fields, metadata}
    override def isOptional: Boolean = true
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, Option[A]] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Record[S, U, Option[A]] =
      copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Root[S[_], T[_], A](field: Field[S, T, A], metadata: Metadata)
      extends Record[S, T, A]:
    override def isOptional: Boolean = false
    override def fields: Chain[Field[S, T, A]] = Chain.one(field)
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, A] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Record[S, U, A] =
      copy(field = field.mapK[T1, U](fK))

  final private[otter] case class Zip[S[_], T[_], A, B](
      left: Record[S, T, A],
      right: Record[S, T, B],
      metadata: Metadata
  ) extends Record[S, T, (A, B)]:
    override def isOptional: Boolean = left.isOptional && right.isOptional
    override def fields: Chain[Field[S, T, ?]] = left.fields ++ right.fields
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, (A, B)] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Record[S, U, (A, B)] =
      copy(left = left.mapK[T1, U](fK), right = right.mapK[T1, U](fK))

  given [Key[_], Value[_]]: RecordSchema[Record[Key, Value, *], Key, Value] with
    override def record[A](field: Field[Key, Value, A]): Record[Key, Value, A] =
      Root(field, metadata = Metadata.Empty)

    extension [A](self: Record[Key, Value, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Record[Key, Value, A] = self.modifyMetadata(f)
      override def imap[B](f: A => B)(g: B => A): Record[Key, Value, B] = self.imap(f)(g)
      override def zip[B](schema: Record[Key, Value, B]): Record[Key, Value, (A, B)] = self.zip(schema)
      override def optional: Record[Key, Value, Option[A]] = self.optional
