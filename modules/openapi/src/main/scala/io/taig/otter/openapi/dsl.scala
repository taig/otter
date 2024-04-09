package io.taig.otter.openapi

import io.taig.otter as Plain
import cats.Id as Identity
import io.taig.otter.Attribute
import io.taig.otter.openapi as OpenApi
import io.taig.otter.Metadatas
import io.taig.otter.Type

final case class Annotation[+S, +M](self: S, metadata: M)

object dsl:
  self =>

  type Schema[A] = Annotation[Plain.Schema[A], Metadata[Identity]]

  type Value[A] = Annotation[Plain.Value[A], Metadata.Value[Identity]]

  type Primitive[A] = Annotation[Plain.Primitive[A], Metadata.Primitive[Identity]]

  object Primitive:
    type Required[A] = Annotation[Plain.Primitive.Required[A], Metadata.Primitive[Identity]]

  type Tuple[A] = Annotation[Plain.Tuple[A], Metadata.Tuple[Identity]]

  def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    Annotation(Plain.Primitive.Required.Root(tpe), Metadata.Primitive.Default)

  val string: Primitive.Required[String] = primitive(Type.String)

  given [A]: Conversion[Schema[A], Plain.Schema.Ops[Schema, Schema, Tuple, A]] = ???

  given [A]: Conversion[Primitive[A], Plain.Primitive.Ops[Primitive, Primitive, Tuple, A]] = ???

  given [A]: Conversion[Primitive.Required[A], Plain.Primitive.Ops[Primitive.Required, Primitive, Tuple, A]] = self =>
    new Plain.Primitive.Ops[Primitive.Required, Primitive, Tuple, A]:
      export self.self.tpe
      override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = self.copy(self = self.self.imap(f)(g))
      override def optional: Primitive[Option[A]] = self.copy(self = self.self.optional)
      override def toTuple: Tuple[A] = Annotation(self = self.self.toTuple, ???)

object Playground:
  import dsl.*
  import dsl.given

  val x: Primitive[String] = string.imap(_.reverse)(_.reverse)
