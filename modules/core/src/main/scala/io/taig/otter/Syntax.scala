package io.taig.otter

import io.taig.otter as Plain

trait Syntax extends Types:
  def toSchemaOps[A](self: Schema[A]): Plain.Schema.Ops[Schema, Schema, Tuple, A]
  def toPrimitiveRequiredOps[A](
      self: Primitive.Required[A]
  ): Plain.Primitive.Ops[Primitive.Required, Primitive, Tuple, A]

  given [A]: Conversion[Schema[A], Plain.Schema.Ops[Schema, Schema, Tuple, A]] = toSchemaOps
  given [A]: Conversion[Primitive.Required[A], Plain.Primitive.Ops[Primitive.Required, Primitive, Tuple, A]] =
    toPrimitiveRequiredOps

  // extension [A](self: Schema[A]) def toTuple: Tuple.Of[self.type, A] = self.toTupleWith(_ => Metadata.tuple)

  // extension [A](self: Tuple[A])
  //   final def product[B](tuple: Tuple[B]): Tuple[(A, B)] = self.productWith((_, _) => Metadata.tuple)(tuple)
  //   final def zip[B](tuple: Tuple[B])(using merge: Evidence.Merge[A, B]): Tuple[merge.Out] =
  //     product(tuple).imap(merge.apply)(merge.unapply)
  //   final def :*[B](schema: Schema[B])(using merge: Evidence.Merge[A, B]): Tuple[merge.Out] = zip(schema.toTuple)
  //   final def *:[B](schema: Schema[B])(using merge: Evidence.Merge[B, A]): Tuple[merge.Out] = schema.toTuple.zip(self)
