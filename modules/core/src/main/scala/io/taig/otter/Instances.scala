package io.taig.otter

import io.taig.otter as Base

trait Instances extends Types:
  def modifySchema[A, B, C, D](schema: Schema.Of[A, B])(
      f: Base.Schema[AsSchema, A, B] => Base.Schema[AsSchema, C, D]
  ): Schema.Of[C, D]

  def modifyPrimitive[A, B](schema: Primitive[A])(
      f: Base.Primitive[A] => Base.Primitive[B]
  ): Primitive[B]

  def modifyPrimitiveRequired[A, B](schema: Primitive.Required[A])(
      f: Base.Primitive.Required[A] => Base.Primitive[B]
  ): Primitive[B]

  final given [A]: SchemaOps[Schema.Of[A, *], Schema.Of[A, *]] =
    new SchemaOps[Schema.Of[A, *], Schema.Of[A, *]]:
      extension [B](self: Schema.Of[A, B])
        override def optional: Schema.Of[A, Option[B]] = modifySchema(self)(_.optional)

  final given PrimitiveOps[Primitive.Required, Primitive] =
    new PrimitiveOps[Primitive.Required, Primitive]:
      extension [A](self: Primitive.Required[A])
        override def optional: Primitive[Option[A]] = modifyPrimitiveRequired(self)(_.optional)
        override def tpe: Base.Type[?] = ???

  final given PrimitiveOps[Primitive, Primitive] = new PrimitiveOps[Primitive, Primitive]:
    extension [A](self: Primitive[A])
      override def optional: Primitive[Option[A]] = modifyPrimitive(self)(_.optional)
      override def tpe: Base.Type[?] = ???
