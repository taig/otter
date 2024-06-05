package io.taig.otter

import io.taig.otter as Base
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

trait Schemas extends Types:
  final def primitive[A](tpe: Type[A]): Primitive.Required[A] = Base.Primitive.Required.Root(tpe)

  final val bigDecimal: Primitive.Required[JBigDecimal] = primitive(Type.BigDecimal)
  final val bigInteger: Primitive.Required[JBigInteger] = primitive(Type.BigInteger)
  final val boolean: Primitive.Required[Boolean] = primitive(Type.Boolean)
  final val double: Primitive.Required[Double] = primitive(Type.Double)
  final val float: Primitive.Required[Float] = primitive(Type.Float)
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)
  final val string: Primitive.Required[String] = primitive(Type.String)
