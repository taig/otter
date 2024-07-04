package io.taig.otter

import io.taig.otter as Base
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import cats.Applicative

trait Schemas extends Instances:
  final def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    Applicative[container.Primitive].pure(Base.Primitive.Required.Root(tpe))

  final val bigDecimal: Primitive.Required[JBigDecimal] = primitive(Type.BigDecimal)
  final val bigInteger: Primitive.Required[JBigInteger] = primitive(Type.BigInteger)
  final val boolean: Primitive.Required[Boolean] = primitive(Type.Boolean)
  final val double: Primitive.Required[Double] = primitive(Type.Double)
  final val float: Primitive.Required[Float] = primitive(Type.Float)
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)
  final val string: Primitive.Required[String] = primitive(Type.String)

  final def branch[A](name: String, schema: Schema[A]): Branch.Of[schema.type, A] =
    Base.Branch.Root(name, schema)
  final def branch[A](name: String, schema: Schema.Reader[A]): Branch.Reader.Of[schema.type, A] =
    Base.Branch.Reader.Root(name, schema)
  final def branch[A](name: String, schema: Schema.Writer[A]): Branch.Writer.Of[schema.type, A] =
    Base.Branch.Writer.Root(name, schema)

  final def field[A](name: String, schema: Schema[A]): Field.Of[schema.type, A] =
    Base.Field.Root(name, Field.Null.Default, schema)
  final def field[A](name: String, schema: Schema.Reader[A]): Field.Reader.Of[schema.type, A] =
    Base.Field.Reader.Root(name, schema)
  final def field[A](name: String, schema: Schema.Writer[A]): Field.Writer.Of[schema.type, A] =
    Base.Field.Writer.Root(name, Field.Null.Default, schema)
