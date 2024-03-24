package io.taig.otter.openapi

import io.taig.otter.Schema
import io.taig.otter.Primitive
import io.taig.otter.Dsl
import io.taig.otter.Type
import io.taig.otter
import io.taig.otter.Types

object dsl extends Dsl {
  override type Primitive[A] = otter.Schema.With[otter.Primitive[A], String]

  override val Primitive = new Types.Primitive {
    override type Required[A] = otter.Schema.With[otter.Primitive.Required[A], String]
  }

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    otter.Schema.With(otter.Primitive.Required.Root(tpe), "")
}

type Metadata[S <: Schema[?]] = S match
  case Primitive[?] => Metadata.Primitive

object Metadata:
  final case class Primitive(format: Option[String])

object OpenApi:
  val x: Primitive[Int] = ???

  val y: Schema.With[x.type, Metadata[x.type]] = ???

  val z: Metadata.Primitive = y.value
