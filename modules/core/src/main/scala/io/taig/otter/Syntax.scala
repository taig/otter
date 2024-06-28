package io.taig.otter

import io.taig.otter as Base
import cats.Functor
import cats.Contravariant
import cats.Comonad
import cats.Applicative
import cats.syntax.all.*

trait Syntax extends Syntax1:
  given primitiveRequiredIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive.Required, Primitive] with
    extension [A](self: Primitive.Required[A])
      override def tpe: Base.Type[?] =
        primitiveApplicativeComonad.extract(self).tpe

    extension [A, B](self: Primitive.Required[B])
      override def collection: Collection.Of[self.type, Vector[B]] =
        collectionApplicativeComonad.pure(Base.Collection.Root(self))
      override def optional: Primitive[Option[B]] = primitiveApplicativeComonad.map(self)(_.optional)
      override def union: Union.Of[self.type, B] = unionApplicativeComonad.pure(Base.Union.Root(self))
      override def ivalidate[C, D, E, F](validation: SchemaValidation.Primitive[B, C, D, E])(
          f: E => B
      ): Primitive.Required[E] = primitiveApplicativeComonad.map(self)(_.ivalidate(validation)(f))

  given primitiveRequiredToFunctorOps[A]: Conversion[
    Primitive.Required[A],
    Functor.Ops[Primitive.Required.Reader, A]
  ] = toFunctorOps[[_, a] =>> Primitive.Required[a], [_, a] =>> Primitive.Required.Reader[a], Nothing, A]

  given primitiveRequiredToContravariantOps[A]: Conversion[
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

  given collectionToFunctorOps[A, B]: Conversion[Collection.Of[A, B], Functor.Ops[Collection.Reader.Of[A, *], B]] =
    toFunctorOps[Collection.Of, Collection.Reader.Of, A, B]

  given collectionToContravariantOps[A, B]
      : Conversion[Collection.Of[A, B], Contravariant.Ops[Collection.Writer.Of[A, *], B]] =
    toContravariantOps[Collection.Of, Collection.Writer.Of, A, B]

  given primitiveIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive, Primitive] = ???

  given primitiveReaderOps: PrimitiveOps.Reader[Primitive.Reader, Primitive.Reader] = ???

  given primitiveWriterOps: PrimitiveOps.Writer[Primitive.Writer, Primitive.Writer] = ???

  given primitiveToFunctorOps[A]: Conversion[Primitive[A], Functor.Ops[Primitive.Reader, A]] =
    toFunctorOps[[_, a] =>> Primitive[a], [_, a] =>> Primitive.Reader[a], Nothing, A]

  given primitiveToContravariantOps[A]: Conversion[Primitive[A], Contravariant.Ops[Primitive.Writer, A]] =
    toContravariantOps[[_, a] =>> Primitive[a], [_, a] =>> Primitive.Writer[a], Nothing, A]

trait Syntax3 extends Instances:
  given schemaIsomoprhicOps: SchemaOps.Isomorphic = ???

  given schemaReaderOps: SchemaOps.Reader = ???

  given schemaWriterOps: SchemaOps.Writer = ???

  given schemaToFunctorOps[A, B]: Conversion[Schema.Of[A, B], Functor.Ops[Schema.Reader.Of[A, *], B]] =
    toFunctorOps[Schema.Of, Schema.Reader.Of, A, B]

  given schemaToContravariantOps[A, B]: Conversion[Schema.Of[A, B], Contravariant.Ops[Schema.Writer.Of[A, *], B]] =
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
