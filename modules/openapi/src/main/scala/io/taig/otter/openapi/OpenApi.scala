package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type
import cats.Id as Identity
import io.taig.otter.Annotation

object OpenApi extends Dsl:
  self =>

  object Metadata extends self.Metadata:
    override type Schema = io.taig.otter.openapi.Metadata[Identity]
    override type Primitive = io.taig.otter.openapi.Metadata.Primitive[Identity]
    override type Product = io.taig.otter.openapi.Metadata.Product[Identity]

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = Annotation(
    Plain.Primitive.Required.Root(tpe),
    io.taig.otter.openapi.Metadata.Primitive[Identity](None, None, None)
  )

  given [S <: Primitive[A], A]
      : Conversion[S, io.taig.otter.openapi.Metadata.Primitive[io.taig.otter.openapi.Metadata.Field[S, *]]] =
    self => self.metadata.toFields(???)

object Playground {
  import OpenApi.*
  import OpenApi.given

  val x: Primitive.Required[String] = string
  val y: Schema[String] = x

  x.name.value
  x.name.apply(_.map(_.reverse))

  // x.name(_.map(_.reverse))

  // x.name
  // x.name.modify(self => Annotation(x.self, self))(_.map(_.reverse))
  // val z: Primitive.Required[String] = x.metadata.name.modify(identity)

  // val a: Primitive.Required[String] = z.imap(_.reverse)(_.reverse)

  // z.name.apply("")
  // z.name.apply(None)
  // val a: Schema[String] = z.name.apply(Some("lol"))
}
