package io.taig.otter

import cats.syntax.all.*

sealed abstract class Field[F <: Data.Any, A] extends Product with Serializable:
  def name: String
  def codec: Codec[?, ?]
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Field[F, A]
  def imap[B](f: A => B)(g: B => A): Field[F, B] = Field.Modify(self = this, f, g)

object Field:
  final case class Required[F <: Data.Any, A](name: String, codec: Codec[F, A], metadata: Metadata) extends Field[F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Field.Required[F, A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Field.Required[F, B] = copy(codec = codec.imap(f)(g))
    def optional: Field[F, Option[A]] = Optional(self = this)
    def optional(default: A): Field[F, A] = Default(self = this, value = default)

  final private[otter] case class Modify[F <: Data.Any, A, B](self: Field[F, A], f: A => B, g: B => A)
      extends Field[F, B]:
    export self.{codec, metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Field[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Default[F <: Data.Any, A](self: Field.Required[F, A], value: A) extends Field[F, A]:
    export self.{codec, metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Field[F, A] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Optional[F <: Data.Any, A](self: Field.Required[F, A]) extends Field[F, Option[A]]:
    export self.{codec, metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Field[F, Option[A]] = copy(self = self.modifyMetadata(f))
