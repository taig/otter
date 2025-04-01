package io.taig.otter

import cats.syntax.all.*

sealed abstract class Field[+S[_], +T[_], A] extends Product with Serializable:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Field[S, T, A]
  def imap[B](f: A => B)(g: B => A): Field[S, T, B] = Field.Modify(self = this, f, g)

object Field:
  sealed abstract class Required[S[_], T[_], A] extends Field[S, T, A]:
    override def modifyMetadata(f: Metadata => Metadata): Field.Required[S, T, A]
    final override def imap[B](f: A => B)(g: B => A): Field.Required[S, T, B] =
      Required.Modify(self = this, f, g)
    def optional: Field[S, T, Option[A]] = Optional(self = this)
    def optional(default: A): Field[S, T, A] = Default(self = this, default)

  object Required:
    final private[otter] case class Modify[S[_], T[_], A, B](self: Field.Required[S, T, A], f: A => B, g: B => A)
        extends Field.Required[S, T, B]:
      export self.{key, metadata, value}
      override def modifyMetadata(f: Metadata => Metadata): Field.Required[S, T, B] =
        copy(self = self.modifyMetadata(f))

    final private[otter] case class Root[S[_], T[_], A, B](
        key: Reference.Constant[S, A],
        value: Reference[T, B],
        metadata: Metadata
    ) extends Field.Required[S, T, B]:
      override def modifyMetadata(f: Metadata => Metadata): Field.Required[S, T, B] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S[_], T[_], A, B](self: Field[S, T, A], f: A => B, g: B => A)
      extends Field[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Default[S[_], T[_], A](self: Field.Required[S, T, A], default: A)
      extends Field[S, T, A]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, A] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Optional[S[_], T[_], A](self: Field.Required[S, T, A]) extends Field[S, T, Option[A]]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, Option[A]] =
      copy(self = self.modifyMetadata(f))
