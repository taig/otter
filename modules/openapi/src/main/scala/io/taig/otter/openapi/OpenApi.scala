package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type

object OpenApi extends Dsl:
  final case class Annotation[+S[a] <: Plain.Schema[a], +M[+_], A](self: S[A], metadata: M[Annotation[S, M, A]])

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

  def primitive[A](tpe: Type[A], attributes: Metadata.Primitive.Attributes): Primitive.Required[A] = Annotation(
    Plain.Primitive.Required.Root(tpe),
    Metadata.Primitive[Primitive.Required[A]](attributes)(primitive(tpe, _))
  )
  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = primitive(tpe, Metadata.Primitive.Default)

object Playground {
  import OpenApi.*

  val x: Primitive.Required[String] = string
  val y: Schema[String] = x

  val z: Primitive.Required[String] = x.metadata.name(???)

  // x.metadata.name

  // val z: Schema[String] = x.name.update(_.map(_.reverse))

}
