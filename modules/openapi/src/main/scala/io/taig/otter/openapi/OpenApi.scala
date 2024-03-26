package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type

object OpenApi extends Dsl:
  override type Schema[A] = Annotation[Plain.Schema, Metadata.Schema, A]

  override val Schema = new Schemas:
    override type Of[S <: Plain.Schema[?], A] = Annotation[Plain.Schema.Of[S, *], Metadata.Schema, A]

  override type Primitive[A] = Annotation[Plain.Primitive, Metadata.Primitive, A]

  override object Primitive extends Primitives:
    override type Required[A] = Annotation[Plain.Primitive.Required, Metadata.Primitive, A]
    override type Optional[A] = Annotation[Plain.Primitive.Optional, Metadata.Primitive, A]

  override type Product[A] = Annotation[Plain.Product, Metadata.Schema, A]

  override val Product = new Products:
    override type Of[S <: Plain.Schema[?], A] = Annotation[Plain.Product.Of[S, *], Metadata.Schema, A]

  given toMetadata[S[a] <: Plain.Schema[a], M[+s] <: Metadata.Schema[s], A]
      : Conversion[Annotation[S, M, A], M[Annotation[S, M, A]]] = _.metadata

  def primitive[A](tpe: Type[A], attributes: Metadata.Primitive.Attributes): Primitive.Required[A] = Annotation(
    Plain.Primitive.Required.Root(tpe),
    Metadata.Primitive[Primitive.Required[A]](attributes)(primitive(tpe, _))
  )
  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = primitive(tpe, Metadata.Primitive.Default)

object Playground {
  import OpenApi.*
  import OpenApi.given

  val x: Primitive.Required[String] = string
  val y: Schema[String] = x

  val z: Primitive.Required[String] = x.metadata.name.modify(identity)

  z.name.apply("")
  z.name.apply(None)
  val a: Schema[String] = z.name.apply(Some("lol"))
}
