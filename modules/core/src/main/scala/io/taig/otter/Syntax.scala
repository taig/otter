package io.taig.otter

trait Syntax extends Types:
  def toOperation[A](self: Schema[A]): Operation[Schema, Schema, Schema, Tuple, A]
  def toOperationPrimitiveRequired[A](
      self: Primitive.Required[A]
  ): Operation.Primitive[Primitive.Required, Primitive, Schema, Tuple, A]

  given [A]: Conversion[Schema[A], Operation[Schema, Schema, Schema, Tuple, A]] = toOperation
  given [A]: Conversion[Primitive.Required[A], Operation.Primitive[Primitive.Required, Primitive, Schema, Tuple, A]] =
    toOperationPrimitiveRequired
