package io.taig.otter

import io.taig.otter as Base
import cats.Functor
import cats.Contravariant
import cats.Comonad

trait Syntax extends Syntax1:
  given primitiveIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive, Primitive] = ???

  given primitiveRequiredIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive.Required, Primitive] = ???

  given primitiveRequiredToFunctorOps[A](using F: Comonad[container.Primitive]): Conversion[
    Primitive.Required[A],
    Functor.Ops[Primitive.Required.Reader, A]
  ] = toFunctorOps[[_, a] =>> Primitive.Required[a], [_, a] =>> Primitive.Required.Reader[a], Nothing, A]

  given primitiveRequiredToContravariantOps[A](using F: Comonad[container.Primitive]): Conversion[
    Primitive.Required[A],
    Contravariant.Ops[Primitive.Required.Writer, A]
  ] = toContravariantOps[[_, a] =>> Primitive.Required[a], [_, a] =>> Primitive.Required.Writer[a], Nothing, A]

trait Syntax1 extends Syntax2:
  given primitiveRequiredReaderOps: PrimitiveOps.Reader[Primitive.Required.Reader, Primitive.Reader] = ???

  given primitiveRequiredWriterOps: PrimitiveOps.Writer[Primitive.Required.Writer, Primitive.Writer] = ???

trait Syntax2 extends Syntax3:
  given primitiveReaderOps: PrimitiveOps.Reader[Primitive.Reader, Primitive.Reader] = ???

  given primitiveWriterOps: PrimitiveOps.Writer[Primitive.Writer, Primitive.Writer] = ???

  given primitiveToFunctorOps[A](using
      F: Comonad[container.Primitive]
  ): Conversion[Primitive[A], Functor.Ops[Primitive.Reader, A]] =
    toFunctorOps[[_, a] =>> Primitive[a], [_, a] =>> Primitive.Reader[a], Nothing, A]

  given primitiveToContravariantOps[A](using
      F: Comonad[container.Primitive]
  ): Conversion[Primitive[A], Contravariant.Ops[Primitive.Writer, A]] =
    toContravariantOps[[_, a] =>> Primitive[a], [_, a] =>> Primitive.Writer[a], Nothing, A]

trait Syntax3 extends Instances:
  given schemaToFunctorOps[A, B](using
      F: Comonad[container.Schema]
  ): Conversion[Schema.Of[A, B], Functor.Ops[Schema.Reader.Of[A, *], B]] =
    toFunctorOps
  given schemaToContravariantOps[A, B](using
      F: Comonad[container.Schema]
  ): Conversion[Schema.Of[A, B], Contravariant.Ops[Schema.Writer.Of[A, *], B]] =
    toContravariantOps

def toFunctorOps[Self[a, b] <: Reader[a, b], Reader[_, _], A, B](using
    F: Functor[Reader[A, *]]
): Conversion[Self[A, B], Functor.Ops[Reader[A, *], B]] = schema =>
  new Functor.Ops[Reader[A, *], B]:
    override type TypeClassType = Functor[Reader[A, *]]
    override def self: Self[A, B] = schema
    override val typeClassInstance: TypeClassType = F

def toContravariantOps[Self[a, b] <: Writer[a, b], Writer[_, _], A, B](using
    F: Contravariant[Writer[A, *]]
): Conversion[Self[A, B], Contravariant.Ops[Writer[A, *], B]] = schema =>
  new Contravariant.Ops[Writer[A, *], B]:
    override type TypeClassType = Contravariant[Writer[A, *]]
    override def self: Self[A, B] = schema
    override val typeClassInstance: TypeClassType = F
