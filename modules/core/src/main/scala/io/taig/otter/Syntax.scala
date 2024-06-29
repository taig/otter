package io.taig.otter

import io.taig.otter as Base
import cats.Functor
import cats.Contravariant

trait Syntax extends Syntax1:
  given primitiveRequiredIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive.Required, Primitive] = ???

  given primitiveRequiredToFunctorOps[A](using Functor[container.Primitive]): Conversion[
    Primitive.Required[A],
    Functor.Ops[Primitive.Required.Reader, A]
  ] = toFunctorOps[[_, a] =>> Primitive.Required[a], [_, a] =>> Primitive.Required.Reader[a], Nothing, A]

  given primitiveRequiredToContravariantOps[A](using Functor[container.Primitive]): Conversion[
    Primitive.Required[A],
    Contravariant.Ops[Primitive.Required.Writer, A]
  ] = toContravariantOps[[_, a] =>> Primitive.Required[a], [_, a] =>> Primitive.Required.Writer[a], Nothing, A]

trait Syntax1 extends Syntax2:
  given primitiveRequiredReaderOps: PrimitiveOps.Reader[Primitive.Required.Reader, Primitive.Reader] = ???

  given primitiveRequiredWriterOps: PrimitiveOps.Writer[Primitive.Required.Writer, Primitive.Writer] = ???

trait Syntax2 extends Syntax3:
  given collectionIsomoprhicOps: CollectionOps.Isomorphic = ???

  given collectionReaderOps: CollectionOps.Reader = ???

  given collectionWriterOps: CollectionOps.Writer = ???

  given collectionToFunctorOps[A, B](using Functor[container.Collection]): Conversion[
    Collection.Of[A, B],
    Functor.Ops[Collection.Reader.Of[A, *], B]
  ] = toFunctorOps[Collection.Of, Collection.Reader.Of, A, B]

  given collectionToContravariantOps[A, B](using Functor[container.Collection]): Conversion[
    Collection.Of[A, B],
    Contravariant.Ops[Collection.Writer.Of[A, *], B]
  ] = toContravariantOps[Collection.Of, Collection.Writer.Of, A, B]

  given primitiveIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive, Primitive] = ???

  given primitiveReaderOps: PrimitiveOps.Reader[Primitive.Reader, Primitive.Reader] = ???

  given primitiveWriterOps: PrimitiveOps.Writer[Primitive.Writer, Primitive.Writer] = ???

  given primitiveToFunctorOps[A]: Conversion[Primitive[A], Functor.Ops[Primitive.Reader, A]] =
    toFunctorOps[[_, a] =>> Primitive[a], [_, a] =>> Primitive.Reader[a], Nothing, A]

  given primitiveToContravariantOps[A]: Conversion[Primitive[A], Contravariant.Ops[Primitive.Writer, A]] =
    toContravariantOps[[_, a] =>> Primitive[a], [_, a] =>> Primitive.Writer[a], Nothing, A]

  given unionIsomoprhicOps: UnionOps.Isomorphic = ???

  given unionReaderOps: UnionOps.Reader = ???

  given unionWriterOps: UnionOps.Writer = ???

  given unionToFunctorOps[A, B](using Functor[container.Union]): Conversion[
    Union.Of[A, B],
    Functor.Ops[Union.Reader.Of[A, *], B]
  ] = toFunctorOps[Union.Of, Union.Reader.Of, A, B]

  given unionToContravariantOps[A, B](using Functor[container.Union]): Conversion[
    Union.Of[A, B],
    Contravariant.Ops[Union.Writer.Of[A, *], B]
  ] = toContravariantOps[Union.Of, Union.Writer.Of, A, B]

trait Syntax3 extends Instances:
  given schemaIsomorphicCoproductLiftOps: CoproductLiftOps[Schema.Of, Schema.Of, Union.Of] = ???

  given schemaReaderCoproductLiftOps: CoproductLiftOps[Schema.Reader.Of, Schema.Reader.Of, Union.Reader.Of] = ???

  given schemaWriterCoproductLiftOps: CoproductLiftOps[Schema.Writer.Of, Schema.Writer.Of, Union.Writer.Of] = ???

  given schemaIsomoprhicOps: SchemaOps.Isomorphic = ???

  given schemaReaderOps: SchemaOps.Reader = ???

  given schemaWriterOps: SchemaOps.Writer = ???

  given schemaToFunctorOps[A, B](using
      Functor[container.Schema]
  ): Conversion[Schema.Of[A, B], Functor.Ops[Schema.Reader.Of[A, *], B]] =
    toFunctorOps[Schema.Of, Schema.Reader.Of, A, B]

  given schemaToContravariantOps[A, B](using
      Functor[container.Schema]
  ): Conversion[Schema.Of[A, B], Contravariant.Ops[Schema.Writer.Of[A, *], B]] =
    toContravariantOps[Schema.Of, Schema.Writer.Of, A, B]

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
