package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type
import cats.Id as Identity

object OpenApi extends Dsl:
  self =>

  override type Schema[A] = Plain.Schema[Metadata[Identity], A]
  override type Primitive[A] = Plain.Primitive[Metadata.Primitive[Identity], A]
  override object Primitive extends Primitives:
    override type Required[A] = Plain.Primitive.Required[Metadata.Primitive[Identity], A]
  override type Product[A] = Plain.Product[Metadata.Product[Identity], A]
  override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    Plain.Primitive.Required.Root(Metadata.Primitive.Default, tpe)

  given [A]: Conversion[Primitive[A], Metadata.Primitive[Metadata.Field[Primitive[A], *]]] =
    self => self.metadata.toFields(f => self.update(_ => f))

object Playground:
  import OpenApi.*
  import OpenApi.given

  string.name
