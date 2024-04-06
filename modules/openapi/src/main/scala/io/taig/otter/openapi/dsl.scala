package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type
import cats.Id as Identity
import io.taig.otter.Attribute
import io.taig.otter.openapi as OpenApi

object dsl extends Dsl:
  self =>

  override type Schema[A] = OpenApi.Schema[A]

  override type Value[A] = OpenApi.Value[A]

  override type Primitive[A] = OpenApi.Primitive[A]

  override object Primitive extends Primitives:
    override type Required[A] = OpenApi.Primitive.Required[A]

  override def primitive[A](tpe: Plain.Type[A]): Primitive.Required[A] =
    OpenApi.Primitive.Required(Metadata.Primitive.Default, Plain.Primitive.Required.Root(tpe))

  given [
      S <: Schema[A] { type M[f[_]] = Mx[f] },
      Mx[f[_]] <: Metadata[f] { type Self[f[_]] = Fx[f] },
      Fx[f[_]] <: Metadata[f],
      A
  ]: Conversion[S, Fx[Attribute[S, *]]] =
    _.metadata.toAttributes(???)

object Playground:
  import dsl.*
  import dsl.given

  val x: Primitive.Required[String] = string.name.clear
