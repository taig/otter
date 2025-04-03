package io.taig.otter

import cats.syntax.all.*
import cats.~>

sealed abstract class Field[+S[_], +T[_], A] extends Product with Serializable:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Field[S, T, A]
  def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Field[S, U, A]
  def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Field[U, T, A]
  def imap[B](f: A => B)(g: B => A): Field[S, T, B] = Field.Modify(self = this, f, g)

  final def toRecord: Record[S, T, A] = Record.Root(field = this, metadata = Metadata.Empty)

object Field:
  sealed abstract class Required[S[_], T[_], A] extends Field[S, T, A]:
    override def modifyMetadata(f: Metadata => Metadata): Field.Required[S, T, A]
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Field.Required[S, U, A]
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Field.Required[U, T, A]
    final override def imap[B](f: A => B)(g: B => A): Field.Required[S, T, B] =
      Required.Modify(self = this, f, g)
    final def optional: Field[S, T, Option[A]] = Optional(self = this)
    final def optional(default: A): Field[S, T, A] = Default(self = this, default)

  object Required:
    final private[otter] case class Modify[S[_], T[_], A, B](self: Field.Required[S, T, A], f: A => B, g: B => A)
        extends Field.Required[S, T, B]:
      export self.{key, metadata, value}
      override def modifyMetadata(f: Metadata => Metadata): Field.Required[S, T, B] =
        copy(self = self.modifyMetadata(f))
      override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Field.Required[S, U, B] = copy(self = self.mapK(fK))
      override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Field.Required[U, T, B] = copy(self = self.leftMapK(fK))

    final private[otter] case class Root[S[_], T[_], A, B](
        key: Reference.Constant[S, A],
        value: Reference[T, B],
        metadata: Metadata
    ) extends Field.Required[S, T, B]:
      override def modifyMetadata(f: Metadata => Metadata): Field.Required[S, T, B] = copy(metadata = f(metadata))
      override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Field.Required[S, U, B] = copy(value = value.mapK(fK))
      override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Field.Required[U, T, B] = copy(key = key.mapK(fK))

  final private[otter] case class Modify[S[_], T[_], A, B](self: Field[S, T, A], f: A => B, g: B => A)
      extends Field[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Field[S, U, B] = copy(self = self.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Field[U, T, B] = copy(self = self.leftMapK(fK))

  final private[otter] case class Default[S[_], T[_], A](self: Field.Required[S, T, A], default: A)
      extends Field[S, T, A]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, A] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Field[S, U, A] = copy(self = self.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Field[U, T, A] = copy(self = self.leftMapK(fK))

  final private[otter] case class Optional[S[_], T[_], A](self: Field.Required[S, T, A]) extends Field[S, T, Option[A]]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, Option[A]] =
      copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Field[S, U, Option[A]] = copy(self = self.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Field[U, T, Option[A]] = copy(self = self.leftMapK(fK))

  given invariant[Record[_], Key[_], Value[_]](using
      RecordInvariant[Record, Field[Key, Value, *]]
  ): FieldInvariant[Field[Key, Value, *], Record, Key, Value] with
    extension [A](self: Field[Key, Value, A])
      override inline def metadata: Metadata = self.metadata
      override inline def imap[B](f: A => B)(g: B => A): Field[Key, Value, B] =
        self.imap(f)(g)

    override def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Field[Key, Value, B] = Field.Required.Root(
      key = Reference.Constant(self = Reference.later(key), value = name),
      value = Reference.later(value),
      metadata = Metadata.Empty
    )
