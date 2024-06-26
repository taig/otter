package io.taig.otter

import io.taig.otter as Base
import cats.syntax.all.*
import cats.Invariant
import cats.Functor
import cats.Contravariant

trait Syntax extends Types:
  val schemaOps: SchemaIsomorphicOps[Schema.Of, Schema, Union.Of] = 
    new SchemaIsomorphicOps[Schema.Of, Schema, Union.Of] {
      override given invariant[A]: Invariant[Union.Of[A, *]] = ???

      extension [A, B](self: Schema.Of[A, B])
        override def orElse[C](schema: Schema[C]): Union.Of[self.type | schema.type, Either[B, C]] = self.orElseWith(metadata.union, schema)

    }
  
  export schemaOps.*

// trait Syntax extends Syntax1:
//   given schemaToInvariantOps[F[a, b] <: G[a, b], G[a, b], A, B](using
//       F: Functor[G[A, *]]
//   ): Conversion[F[A, B], Invariant.AllOps[G[A, *], B]] = fa =>
//     new Invariant.AllOps:
//       override type TypeClassType = Functor[G[A, *]]
//       override val typeClassInstance: TypeClassType = F
//       override def self: F[A, B] = fa

// trait Syntax1 extends Instances:
//   given schemaToFunctorOps[F[a, b] <: G[a, b], G[a, b], A, B](using
//       F: Functor[G[A, *]]
//   ): Conversion[F[A, B], Functor.AllOps[G[A, *], B]] = fa =>
//     new Functor.AllOps:
//       override type TypeClassType = Functor[G[A, *]]
//       override val typeClassInstance: TypeClassType = F
//       override def self: F[A, B] = fa

//   given schemaToContravariantOps[F[a, b] <: G[a, b], G[a, b], A, B](using
//       F: Contravariant[G[A, *]]
//   ): Conversion[F[A, B], Contravariant.AllOps[G[A, *], B]] = fa =>
//     new Contravariant.AllOps:
//       override type TypeClassType = Contravariant[G[A, *]]
//       override val typeClassInstance: TypeClassType = F
//       override def self: F[A, B] = fa
