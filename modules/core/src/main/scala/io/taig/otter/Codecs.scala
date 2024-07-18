package io.taig.otter

import io.taig.otter as Base

trait Codecs extends Types:
  final def primitive[A](tpe: Type[A]): Primitive.Required[A] = Base.Primitive(tpe)

  val double: Primitive.Required[Double] = primitive(Type.Double)
  val int: Primitive.Required[Int] = primitive(Type.Int)
  val long: Primitive.Required[Long] = primitive(Type.Long)
  val string: Primitive.Required[String] = primitive(Type.String)

object Codecs extends Codecs
