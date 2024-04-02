package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type
import cats.Id as Identity
import io.taig.otter.Annotation

object OpenApi extends Dsl:
  self =>

  object metadata extends self.Metadata:
    override type Schema = io.taig.otter.openapi.Metadata[Identity]
    override type Primitive = Metadata.Primitive[Identity]
    override type Product = Metadata.Product[Identity]

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = Annotation(
    Plain.Primitive.Required.Root(tpe),
    io.taig.otter.openapi.Metadata.Primitive[Identity](None, None, None)
  )

  given [S <: Plain.Primitive[A], A]: Conversion[
    Annotation[S, Metadata.Primitive[Identity]],
    Metadata.Primitive[Metadata.Field[Annotation[S, Metadata.Primitive[Identity]], *]]
  ] =
    self => self.metadata.toFields(metadata => self.copy(metadata = metadata))

object Playground {
  import OpenApi.*
  import OpenApi.given

  val x: Primitive.Required[String] = string.name.clear
  val y: Schema[String] = x
}
