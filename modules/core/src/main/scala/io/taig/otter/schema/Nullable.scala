package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Reference
import cats.~>
import cats.syntax.all.*
import io.taig.otter.Shape

sealed abstract class Nullable[+S[_], A] extends Schema[S, A]:
  def schema: Option[Reference[S, ?]]

  override def modifyMetadata(f: Metadata => Metadata): Nullable[S, A]
  override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, A]
  final override def imap[B](f: A => B)(g: B => A): Nullable[S, B] = Nullable.Modify(self = this, f, g)

object Nullable:
  final private[otter] case class Modify[S[_], A, B](self: Nullable[S, A], f: A => B, g: B => A) extends Nullable[S, B]:
    export self.{metadata, schema}
    override def modifyMetadata(f: Metadata => Metadata): Nullable[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, B] = copy(self = self.mapK[S1, T](fK))

  final private[otter] case class Default[S[_], A](reference: Reference[S, A], default: A, metadata: Metadata)
      extends Nullable[S, A]:
    override def schema: Option[Reference[S, ?]] = reference.some
    override def modifyMetadata(f: Metadata => Metadata): Nullable[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, A] = copy(reference = reference.mapK[S1, T](fK))

  final private[otter] case class Root[S[_], A](reference: Reference[S, A], metadata: Metadata)
      extends Nullable[S, Option[A]]:
    override def schema: Option[Reference[S, ?]] = reference.some
    override def modifyMetadata(f: Metadata => Metadata): Nullable[S, Option[A]] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, Option[A]] = copy(reference = reference.mapK[S1, T](fK))

  final private[otter] case class Void(metadata: Metadata) extends Nullable[Nothing, Unit]:
    override def schema: Option[Reference[Nothing, ?]] = none
    override def modifyMetadata(f: Metadata => Metadata): Nullable[Nothing, Unit] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: Nothing, T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, Unit] = this

  trait Component[+Self[_], -Value[_]](using self: Shape.Nullable[Self, Value]):
    final def nullable[A](schema: => Value[A]): Self[Option[A]] = self.nullable(schema)
    final def nullable[A](schema: => Value[A], default: A): Self[A] = self.nullable(schema, default)
    final def void: Self[Unit] = self.void

  given [Value[_]]: Shape.Nullable[Nullable[Value, *], Value] with
    extension [A](self: Nullable[Value, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Nullable[Value, A] = self.modifyMetadata(f)
      override def imap[B](f: A => B)(g: B => A): Nullable[Value, B] = self.imap(f)(g)

    override def nullable[A](schema: => Value[A]): Nullable[Value, Option[A]] =
      Root(reference = Reference.later(schema), metadata = Metadata.Empty)
    override def nullable[A](schema: => Value[A], default: A): Nullable[Value, A] =
      Default(reference = Reference.later(schema), default, metadata = Metadata.Empty)
    override def void: Nullable[Nothing, Unit] = Nullable.Void(Metadata.Empty)
