package io.taig.otter

import cats.syntax.all.*
import cats.Eval

sealed abstract class Field[S <: Data.Any, A] extends Product with Serializable:
  def name: String
  def codec: Eval[Codec[S, ?]]
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Field[S, A]
  def imap[B](f: A => B)(g: B => A): Field[S, B] = Field.Modify(self = this, f, g)

object Field:
  sealed abstract class Required[S <: Data.Any, A] extends Field[S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Field.Required[S, A]
    final override def imap[B](f: A => B)(g: B => A): Field.Required[S, B] =
      Required.Modify(self = this, f, g)
    def optional: Field[S, Option[A]] = Optional(self = this)
    def optional(default: A): Field[S, A] = Default(self = this, value = default)

  object Required:
    final private[otter] case class Modify[S <: Data.Any, A, B](self: Field.Required[S, A], f: A => B, g: B => A)
        extends Field.Required[S, B]:
      export self.{codec, metadata, name}
      override def modifyMetadata(f: Metadata => Metadata): Field.Required[S, B] = copy(self = self.modifyMetadata(f))

    final private[otter] case class Root[S <: Data.Any, A](name: String, codec: Eval[Codec[S, A]], metadata: Metadata)
        extends Field.Required[S, A]:
      override def modifyMetadata(f: Metadata => Metadata): Field.Required[S, A] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S <: Data.Any, A, B](self: Field[S, A], f: A => B, g: B => A)
      extends Field[S, B]:
    export self.{codec, metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Default[S <: Data.Any, A](self: Field.Required[S, A], value: A) extends Field[S, A]:
    export self.{codec, metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, A] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Optional[S <: Data.Any, A](self: Field.Required[S, A]) extends Field[S, Option[A]]:
    export self.{codec, metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, Option[A]] = copy(self = self.modifyMetadata(f))
