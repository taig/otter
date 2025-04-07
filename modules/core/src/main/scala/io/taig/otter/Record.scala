package io.taig.otter

import cats.data.Chain
import cats.~>

sealed abstract class Record[+S[_], +T[_], A] extends Codec[T, A]:
  def isOptional: Boolean
  def fields: Chain[(Reference.Constant[S, ?], Reference[T, ?])]
  override def modifyMetadata(f: Metadata => Metadata): Record[S, T, A]
  override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Record[S, U, A]
  final override def imap[B](f: A => B)(g: B => A): Record[S, T, B] = Record.Modify(self = this, f, g)
  final def optional: Record[S, T, Option[A]] = Record.Optional(self = this)
  final def zip[S1[a] >: S[a], T1[a] >: T[a], B](codec: Record[S1, T1, B]): Record[S1, T1, (A, B)] =
    Record.Zip(left = this, right = codec, metadata = Metadata.Empty)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Nothing, Unit]:
    override def isOptional: Boolean = false
    override def fields: Chain[Nothing] = Chain.empty
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Nothing, Unit] = copy(metadata = f(metadata))
    override def mapK[T1[_] >: Nothing, U[_]](fK: T1 ~> U): Record[Nothing, U, Unit] = this

  final private[otter] case class Field[S[_], T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Record[S, T, B]:
    override def isOptional: Boolean = false
    override def fields: Chain[(Reference.Constant[S, A], Reference[T, B])] = Chain.one((key, value))
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Record[S, U, B] = copy(value = value.mapK(fK))

  final private[otter] case class Modify[S[_], T[_], A, B](self: Record[S, T, A], f: A => B, g: B => A)
      extends Record[S, T, B]:
    export self.{fields, isOptional, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Record[S, U, B] = copy(self = self.mapK(fK))

  final private[otter] case class Optional[S[_], T[_], A](self: Record[S, T, A]) extends Record[S, T, Option[A]]:
    export self.{fields, metadata}
    override def isOptional: Boolean = true
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, Option[A]] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Record[S, U, Option[A]] = copy(self = self.mapK(fK))

  final private[otter] case class Zip[S[_], T[_], A, B](
      left: Record[S, T, A],
      right: Record[S, T, B],
      metadata: Metadata
  ) extends Record[S, T, (A, B)]:
    override def isOptional: Boolean = left.isOptional && right.isOptional
    override def fields: Chain[(Reference.Constant[S, ?], Reference[T, ?])] = left.fields ++ right.fields
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, (A, B)] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Record[S, U, (A, B)] =
      copy(left = left.mapK(fK), right = right.mapK(fK))

  trait Syntax[Self[_], Key[_], Value[_]] extends Codec.Syntax[Self], Invariant.Product[Self, Self]:
    def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]
    def empty: Self[Unit]

    extension [A](self: Self[A])
      def isOptional: Boolean
      def optional: Self[Option[A]]
      def fields: Chain[(Reference.Constant[Key, ?], Reference[Value, ?])]

  object Syntax:
    trait Default[Self[_], Key[_], Value[_]] extends Syntax[Self, Key, Value]:
      def fromRecord[A](record: Record[Key, Value, A]): Self[A]
      def toRecord[A](self: Self[A]): Record[Key, Value, A]

      final override def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] = fromRecord(
        Record.Field(
          key = Reference.Constant(self = Reference.later(key), value = name),
          value = Reference.later(value),
          metadata = Metadata.Empty
        )
      )
      final override val empty: Self[Unit] = fromRecord(Record.Empty(metadata = Metadata.Empty))

      extension [A](self: Self[A])
        final override def isOptional: Boolean = toRecord(self).isOptional
        final override def optional: Self[Option[A]] = fromRecord(toRecord(self).optional)
        final override def fields: Chain[(Reference.Constant[Key, ?], Reference[Value, ?])] =
          toRecord(self).fields
