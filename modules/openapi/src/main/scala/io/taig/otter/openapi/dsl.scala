package io.taig.otter.openapi

import io.taig.otter as Plain
import cats.Id as Identity
import io.taig.otter.Attribute
import io.taig.otter.openapi as OpenApi
import io.taig.otter.Metadatas
import io.taig.otter.Type
import io.taig.otter.Dsl

final case class Annotation[+S, +M](self: S, metadata: M)

object dsl extends Dsl:
  self =>

  override type Schema[A] = Annotation[Plain.Schema[A], Metadata[Identity]]

  override type Value[A] = Annotation[Plain.Value[A], Metadata.Value[Identity]]

  override type Primitive[A] = Annotation[Plain.Primitive[A], Metadata.Primitive[Identity]]

  override object Primitive extends Primitives:
    type Required[A] = Annotation[Plain.Primitive.Required[A], Metadata.Primitive[Identity]]

  override type Tuple[A] = Annotation[Plain.Tuple[A], Metadata.Tuple[Identity]]

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    Annotation(Plain.Primitive.Required.Root(tpe), Metadata.Primitive.Default)

  override def toSchemaOps[A](
      self: Annotation[Plain.Schema[A], Metadata[Identity]]
  ): Plain.Schema.Ops[Schema, Schema, Tuple, A] = ???

  override def toPrimitiveRequiredOps[A](
      self: Annotation[Plain.Primitive.Required[A], Metadata.Primitive[Identity]]
  ): Plain.Primitive.Ops[Primitive.Required, Primitive, Tuple, A] = new Plain.Primitive.Ops:
    export self.self.tpe
    override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = ???
    override def optional: Primitive[Option[A]] = self.copy(self = self.self.optional)
    override def toTuple: Tuple[A] = Annotation(self.self.toTuple, Metadata.Tuple.Default)

object Playground:
  import dsl.*
  import dsl.given

  val x: Primitive[String] = string.imap(_.reverse)(_.reverse)
