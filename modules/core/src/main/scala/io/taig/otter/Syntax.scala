package io.taig.otter

import cats.Functor
import cats.Contravariant

trait Syntax extends Instances:
  // implicit def schemaToFunctorOps[A, B](fa: Schema.Of[A, B]): Functor.Ops[Schema.Reader.Of[A, *], B] {
  //   type TypeClassType = Functor[Schema.Reader.Of[A, *]]
  // } = new Functor.Ops[Schema.Reader.Of[A, *], B]:
  //   type TypeClassType = Functor[Schema.Reader.Of[A, *]]
  //   override val typeClassInstance: Functor[Schema.Reader.Of[A, *]] = Functor[Schema.Reader.Of[A, *]]
  //   override def self: Schema.Of[A, B] = fa

  given [A, B]: Conversion[Schema.Of[A, B], ?] = ???

  implicit val primitiveRequiredOps: PrimitiveOps[Primitive.Required, Primitive, Collection.Of, Tuple.Of] = ???
