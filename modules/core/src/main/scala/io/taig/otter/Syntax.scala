package io.taig.otter

import io.taig.otter as Base
import cats.Functor
import cats.Contravariant
import cats.Id
import cats.syntax.all.*
import cats.Invariant
import cats.Applicative
import cats.Comonad

trait Syntax extends Syntax1:
  implicit def primitiveRequiredIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive.Required, Primitive] =
    new PrimitiveOps.Isomorphic[Primitive.Required, Primitive]:
      extension [A, B](self: Primitive.Required[B]) override def toPlain: Base.Schema[Id, A, B] = ???

      extension [A](self: Primitive.Required[A]) override def tpe: Base.Type[?] = ???

      extension [A, B](self: Primitive.Required[B])
        override def collection: Collection.Of[self.type, Vector[B]] =
          Applicative[container.Collection].pure(Base.Collection.Root(self))

      extension [A, B](self: Primitive.Required[B]) override def optional: Primitive[Option[B]] = ???

      extension [A, B](self: Primitive.Required[B])
        override def union: Union.Of[self.type, B] =
          Applicative[container.Union].pure(Base.Union.Root(self))

  implicit def primitiveRequiredToFunctorOps[A]: Conversion[
    Primitive.Required[A],
    Functor.Ops[Primitive.Required.Reader, A]
  ] = toFunctorOps[[_, a] =>> Primitive.Required[a], [_, a] =>> Primitive.Required.Reader[a], Nothing, A]

  implicit def primitiveRequiredToContravariantOps[A]: Conversion[
    Primitive.Required[A],
    Contravariant.Ops[Primitive.Required.Writer, A]
  ] = toContravariantOps[[_, a] =>> Primitive.Required[a], [_, a] =>> Primitive.Required.Writer[a], Nothing, A]

trait Syntax1 extends Syntax2:
  implicit def primitiveRequiredReaderOps: PrimitiveOps.Reader[Primitive.Required.Reader, Primitive.Reader] = ???

  implicit def primitiveRequiredWriterOps: PrimitiveOps.Writer[Primitive.Required.Writer, Primitive.Writer] =
    new PrimitiveOps.Writer[Primitive.Required.Writer, Primitive.Writer]:
      extension [A](self: Primitive.Required.Writer[A]) override def tpe: Base.Type[?] = ???

      extension [A, B](self: Primitive.Required.Writer[B])
        override def collection: Collection.Writer.Of[self.type, Vector[B]] = ???
        override def optional: Primitive.Writer[Option[B]] = ???
        override def union: Union.Writer.Of[self.type, B] = ???
        override def toPlain: Base.Schema.Writer[Id, A, B] = ???

trait Syntax2 extends Syntax3:
  implicit def collectionIsomoprhicOps: CollectionOps.Isomorphic = ???

  implicit def collectionReaderOps: CollectionOps.Reader = ???

  implicit def collectionWriterOps: CollectionOps.Writer = ???

  implicit def collectionToFunctorOps[A, B]: Conversion[
    Collection.Of[A, B],
    Functor.Ops[Collection.Reader.Of[A, *], B]
  ] = toFunctorOps[Collection.Of, Collection.Reader.Of, A, B]

  implicit def collectionToContravariantOps[A, B]: Conversion[
    Collection.Of[A, B],
    Contravariant.Ops[Collection.Writer.Of[A, *], B]
  ] = toContravariantOps[Collection.Of, Collection.Writer.Of, A, B]

  implicit def primitiveIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive, Primitive] = ???

  implicit def primitiveReaderOps: PrimitiveOps.Reader[Primitive.Reader, Primitive.Reader] = ???

  implicit def primitiveWriterOps: PrimitiveOps.Writer[Primitive.Writer, Primitive.Writer] = ???

  implicit def primitiveToFunctorOps[A]: Conversion[Primitive[A], Functor.Ops[Primitive.Reader, A]] =
    toFunctorOps[[_, a] =>> Primitive[a], [_, a] =>> Primitive.Reader[a], Nothing, A]

  implicit def primitiveToContravariantOps[A]: Conversion[Primitive[A], Contravariant.Ops[Primitive.Writer, A]] =
    toContravariantOps[[_, a] =>> Primitive[a], [_, a] =>> Primitive.Writer[a], Nothing, A]

  implicit def unionIsomoprhicOps: UnionOps.Isomorphic = new UnionOps.Isomorphic:

    override given selfInvariant[A]: Invariant[Union.Of[A, *]] = ???

    extension [A, B](self: Union.Of[A, B])
      override def orElse[C, D](other: Union.Of[C, D]): Union.Of[A | C, Either[B, D]] =
        val left = Comonad[container.Union].extract(self)
        val right = Comonad[container.Union].extract(other)
        Base.Union.OrElse(left, right).pure[container.Union]
      override def or[C, D](other: Schema.Of[C, D]): Union.Of[A | other.type, Either[B, D]] = orElse(other.union)
      override def toPlain: Base.Schema[Id, A, B] = ???
      override def collection: Collection.Of[self.type, Vector[B]] = ???
      override def optional: Union.Of[A, Option[B]] = ???
      override def union: Union.Of[self.type, B] = ???

  implicit def unionReaderOps: UnionOps.Reader = ???

  implicit def unionWriterOps: UnionOps.Writer = ???

  implicit def unionToFunctorOps[A, B]: Conversion[
    Union.Of[A, B],
    Functor.Ops[Union.Reader.Of[A, *], B]
  ] = toFunctorOps[Union.Of, Union.Reader.Of, A, B]

  implicit def unionToContravariantOps[A, B]: Conversion[
    Union.Of[A, B],
    Contravariant.Ops[Union.Writer.Of[A, *], B]
  ] = toContravariantOps[Union.Of, Union.Writer.Of, A, B]

trait Syntax3 extends Syntax4:
  implicit def schemaIsomorphicCoproductLiftOps: CoproductLiftOps[Schema.Of, Schema.Of, Union.Of] =
    new CoproductLiftOps[Schema.Of, Schema.Of, Union.Of]:
      override given resultInvariant[A]: Invariant[Union.Of[A, *]] = ???

      extension [A, B](self: Schema.Of[A, B])
        override def or[C, D](other: Schema.Of[C, D]): Union.Of[self.type | other.type, Either[B, D]] =
          val left = Comonad[container.Union].extract(self.union)
          val right = Comonad[container.Union].extract(other.union)
          Applicative[container.Union].pure(Base.Union.OrElse(???, ???))

  implicit def schemaIsomoprhicOps: SchemaOps.Isomorphic = new SchemaOps.Isomorphic:
    extension [A, B](self: Schema.Of[A, B])
      override def toPlain: Base.Schema[Id, A, B] = ???
      override def collection: Collection.Of[self.type, Vector[B]] = ???
      override def optional: Schema.Of[A, Option[B]] = ???
      override def union: Union.Of[self.type, B] = Applicative[container.Union].pure(Base.Union.Root(self))

  implicit def schemaReaderOps: SchemaOps.Reader = ???

  implicit def schemaWriterOps: SchemaOps.Writer = ???

  implicit def schemaToFunctorOps[A, B]: Conversion[Schema.Of[A, B], Functor.Ops[Schema.Reader.Of[A, *], B]] =
    toFunctorOps[Schema.Of, Schema.Reader.Of, A, B]

  implicit def schemaToContravariantOps[A, B]
      : Conversion[Schema.Of[A, B], Contravariant.Ops[Schema.Writer.Of[A, *], B]] =
    toContravariantOps[Schema.Of, Schema.Writer.Of, A, B]

trait Syntax4 extends Instances:
  implicit def schemaReaderCoproductLiftOps: CoproductLiftOps[Schema.Reader.Of, Schema.Reader.Of, Union.Reader.Of] = ???

  implicit def schemaWriterCoproductLiftOps: CoproductLiftOps[Schema.Writer.Of, Schema.Writer.Of, Union.Writer.Of] = ???

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
