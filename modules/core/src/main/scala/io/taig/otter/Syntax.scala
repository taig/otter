package io.taig.otter

import io.taig.otter as Base
import cats.Functor
import cats.Contravariant

trait Syntax extends Types
// given schemaToFunctorOps[Self[a, b] <: Reader[a, b], Reader[_, _], A, B](using
//     F: SchemaInvariant[Self, Reader, ?, ?]
// ): Conversion[Self[A, B], Functor.Ops[Reader[A, *], B]] = schema =>
//   new Functor.Ops:
//     override type TypeClassType = Functor[Reader[A, *]]
//     override def self: Reader[A, B] = schema
//     override val typeClassInstance: TypeClassType = F.functor[A]

// given schemaToContravariantOps[Self[a, b] <: Writer[a, b], Writer[_, _], A, B](using
//     F: SchemaInvariant[Self, ?, Writer, ?]
// ): Conversion[Self[A, B], Contravariant.Ops[Writer[A, *], B]] = schema =>
//   new Contravariant.Ops:
//     override type TypeClassType = Contravariant[Writer[A, *]]
//     override def self: Writer[A, B] = schema
//     override val typeClassInstance: TypeClassType = F.contravariant[A]

// given primtiveToFunctorOps[Self[a] <: Reader[a], Reader[_], A](using F: SchemaInvariant[[_, a] =>> Self[a], [_, a] =>> Reader[a], ?, ?]): Conversion[Self[A], Functor.Ops[Reader, A]] = ???
