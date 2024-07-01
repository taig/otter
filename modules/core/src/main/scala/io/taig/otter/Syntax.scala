package io.taig.otter

import io.taig.otter as Base
import cats.Functor
import cats.Contravariant
import cats.Id as Identity
import cats.syntax.all.*
import cats.Invariant
import cats.Applicative
import cats.Comonad

trait Syntax extends Syntax1:
  implicit val primitiveRequiredIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive.Required, Primitive, Base.Primitive.Required] =
    new PrimitiveOps.Isomorphic[Primitive.Required, Primitive, Base.Primitive.Required]:
      extension [A, B](self:Primitive.Required[B]) override def toPlain: Base.Primitive.Required[B] =
        Comonad[container.Schema].extract(self)
      extension [A, B](self: Primitive.Required[B])


        override def optional: Primitive[Option[B]] = Functor[container.Primitive].map(self)(_.optional)
        

// implicit def primitiveRequiredToFunctorOps[A]: Conversion[
//   Primitive.Required[A],
//   Functor.Ops[Primitive.Required.Reader, A]
// ] = toFunctorOps[[_, a] =>> Primitive.Required[a], [_, a] =>> Primitive.Required.Reader[a], Nothing, A]

// implicit def primitiveRequiredToContravariantOps[A]: Conversion[
//   Primitive.Required[A],
//   Contravariant.Ops[Primitive.Required.Writer, A]
// ] = toContravariantOps[[_, a] =>> Primitive.Required[a], [_, a] =>> Primitive.Required.Writer[a], Nothing, A]

trait Syntax1 extends Syntax2:
  implicit val primitiveRequiredReaderOps: PrimitiveOps.Reader[Primitive.Required.Reader, Primitive.Reader, Base.Primitive.Reader] =
    new PrimitiveOps.Reader[Primitive.Required.Reader, Primitive.Reader, Base.Primitive.Reader]:
      extension [A, B](self: Primitive.Required.Reader[B])
        override def optional: Primitive.Reader[Option[B]] = Functor[container.Primitive].map(self)(_.optional)

  implicit val primitiveRequiredWriterOps: PrimitiveOps.Writer[Primitive.Required.Writer, Primitive.Writer, Base.Primitive.Writer] =
    new PrimitiveOps.Writer[Primitive.Required.Writer, Primitive.Writer, Base.Primitive.Writer]:
      extension [A, B](self: Primitive.Required.Writer[B])
        override def optional: Primitive.Writer[Option[B]] = Functor[container.Primitive].map(self)(_.optional)

trait Syntax2 extends Syntax3:
  implicit val primitiveIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive, Primitive, Base.Primitive] =
    new PrimitiveOps.Isomorphic[Primitive, Primitive, Base.Primitive]:
      extension [A, B](self: Primitive[B])
        override def optional: Primitive[Option[B]] = Functor[container.Primitive].map(self)(_.optional)

// implicit def collectionIsomoprhicOps: CollectionOps.Isomorphic = ???

// implicit def collectionReaderOps: CollectionOps.Reader = ???

// implicit def collectionWriterOps: CollectionOps.Writer = ???

// implicit def collectionToFunctorOps[A, B]: Conversion[
//   Collection.Of[A, B],
//   Functor.Ops[Collection.Reader.Of[A, *], B]
// ] = toFunctorOps[Collection.Of, Collection.Reader.Of, A, B]

// implicit def collectionToContravariantOps[A, B]: Conversion[
//   Collection.Of[A, B],
//   Contravariant.Ops[Collection.Writer.Of[A, *], B]
// ] = toContravariantOps[Collection.Of, Collection.Writer.Of, A, B]

// implicit val primitiveIsomoprhicOps: PrimitiveOps.Isomorphic[Primitive, Primitive] =
//   new PrimitiveOps.Isomorphic[Primitive, Primitive]:
//     extension [A](self: Primitive[A])
//       override def tpe: Base.Type[?] = Comonad[container.Primitive].extract(self).tpe

//     extension [A, B](self: Primitive[B])
//       override def toPlain: Base.Schema[Id, ?, B] = Comonad[container.Primitive].extract(self)
//       override def collection: Collection.Of[self.type, Vector[B]] =
//         Applicative[container.Collection].pure(Base.Collection.Root(self))
//       override def optional: Primitive[Option[B]] =
//         Functor[container.Primitive].map(self)(_.optional)
//       override def union: Union.Of[self.type, B] =
//         Applicative[container.Union].pure(Base.Union.Root(self))

// implicit def primitiveReaderOps: PrimitiveOps.Reader[Primitive.Reader, Primitive.Reader] = ???

// implicit def primitiveWriterOps: PrimitiveOps.Writer[Primitive.Writer, Primitive.Writer] = ???

// implicit def primitiveToFunctorOps[A]: Conversion[Primitive[A], Functor.Ops[Primitive.Reader, A]] =
//   toFunctorOps[[_, a] =>> Primitive[a], [_, a] =>> Primitive.Reader[a], Nothing, A]

// implicit def primitiveToContravariantOps[A]: Conversion[Primitive[A], Contravariant.Ops[Primitive.Writer, A]] =
//   toContravariantOps[[_, a] =>> Primitive[a], [_, a] =>> Primitive.Writer[a], Nothing, A]

  implicit val unionIsomoprhicOps: UnionOps.Isomorphic = new UnionOps.Isomorphic:
    override given selfInvariant[A]: Invariant[Union.Of[A, *]] = unionInvariant

    override def lift[A, B](value: Schema.Of[A, B]): Union.Of[value.type, B] =
      Applicative[container.Union].pure(Base.Union.Root(value))

    extension [A, B](self: Union.Of[A, B])
      override def orElse[C, D](other: Union.Of[C, D]): Union.Of[A | C, Either[B, D]] =
        Functor[container.Union].map(self)(_.orElse(Comonad[container.Union].extract(other)))
      override def optional: Union.Of[A, Option[B]] = Functor[container.Union].map(self)(_.optional)

// implicit def unionToFunctorOps[A, B]: Conversion[
//   Union.Of[A, B],
//   Functor.Ops[Union.Reader.Of[A, *], B]
// ] = toFunctorOps[Union.Of, Union.Reader.Of, A, B]

// implicit def unionToContravariantOps[A, B]: Conversion[
//   Union.Of[A, B],
//   Contravariant.Ops[Union.Writer.Of[A, *], B]
// ] = toContravariantOps[Union.Of, Union.Writer.Of, A, B]

trait Syntax3 extends Syntax4:
  implicit val primitiveReaderOps: PrimitiveOps.Reader[Primitive.Reader, Primitive.Reader, Base.Primitive.Reader] =
    new PrimitiveOps.Reader[Primitive.Reader, Primitive.Reader, Base.Primitive.Reader]:
      extension [A, B](self: Primitive.Reader[B])
        override def optional: Primitive.Reader[Option[B]] = Functor[container.Primitive].map(self)(_.optional)

  implicit val primitiveWriterOps: PrimitiveOps.Writer[Primitive.Writer, Primitive.Writer, Base.Primitive.Writer] =
    new PrimitiveOps.Writer[Primitive.Writer, Primitive.Writer, Base.Primitive.Writer]:
      extension [A, B](self: Primitive.Writer[B])
        override def optional: Primitive.Writer[Option[B]] = Functor[container.Primitive].map(self)(_.optional)

  implicit val unionReaderOps: UnionOps.Reader = new UnionOps.Reader:
    override given selfInvariant[A]: Invariant[Union.Reader.Of[A, *]] = unionFunctor

    override def lift[A, B](value: Schema.Reader.Of[A, B]): Union.Reader.Of[value.type, B] =
      Applicative[container.Union].pure(Base.Union.Reader.Root(value))

    extension [A, B](self: Union.Reader.Of[A, B])
      override def orElse[C, D](other: Union.Reader.Of[C, D]): Union.Reader.Of[A | C, Either[B, D]] =
        Functor[container.Union].map(self)(_.orElse(Comonad[container.Union].extract(other)))
      override def optional: Union.Reader.Of[A, Option[B]] = Functor[container.Union].map(self)(_.optional)

  implicit val unionWriterOps: UnionOps.Writer = new UnionOps.Writer:
    override given selfInvariant[A]: Invariant[Union.Writer.Of[A, *]] = unionContravariant

    override def lift[A, B](value: Schema.Writer.Of[A, B]): Union.Writer.Of[value.type, B] =
      Applicative[container.Union].pure(Base.Union.Writer.Root(value))

    extension [A, B](self: Union.Writer.Of[A, B])
      override def orElse[C, D](other: Union.Writer.Of[C, D]): Union.Writer.Of[A | C, Either[B, D]] =
        Functor[container.Union].map(self)(_.orElse(Comonad[container.Union].extract(other)))
      override def optional: Union.Writer.Of[A, Option[B]] = Functor[container.Union].map(self)(_.optional)

  implicit val schemaIsomorphicCoproductLiftOps: CoproductLiftOps[Schema.Of, Schema.Of, Union.Of] =
    new CoproductLiftOps[Schema.Of, Schema.Of, Union.Of]:
      override given resultInvariant[A]: Invariant[Union.Of[A, *]] = unionInvariant

      extension [A, B](self: Schema.Of[A, B])
        override def or[C, D](other: Schema.Of[C, D]): Union.Of[self.type | other.type, Either[B, D]] =
          Applicative[container.Union].pure(
            Base.Union.OrElse[container.Schema, self.type, B, other.type, D](
              left = Base.Union.Root(self),
              right = Base.Union.Root(other)
            )
          )

  implicit val schemaIsomoprhicOps: SchemaOps.Isomorphic[Schema.Of, Schema.Of, Base.Schema[Identity, ?, *]] =
    new SchemaOps.Isomorphic[Schema.Of, Schema.Of, Base.Schema[Identity, ?, *]]:
      extension [A, B](self: Schema.Of[A, B])
        override def optional: Schema.Of[A, Option[B]] = Functor[container.Schema].map(self)(_.optional)

  implicit def schemaToFunctorOps[A, B]: Conversion[Schema.Of[A, B], Functor.Ops[Schema.Reader.Of[A, *], B]] =
    toFunctorOps[Schema.Of, Schema.Reader.Of, A, B]

  implicit def schemaToContravariantOps[A, B]
      : Conversion[Schema.Of[A, B], Contravariant.Ops[Schema.Writer.Of[A, *], B]] =
    toContravariantOps[Schema.Of, Schema.Writer.Of, A, B]

trait Syntax4 extends Syntax5:
  implicit val schemaReaderOps: SchemaOps.Reader[Schema.Reader.Of, Schema.Reader.Of, Base.Schema.Reader[Identity, ?, *]] =
    new SchemaOps.Reader[Schema.Reader.Of, Schema.Reader.Of, Base.Schema.Reader[Identity, ?, *]]:
      extension [A, B](self: Schema.Reader.Of[A, B])
        override def optional: Schema.Reader.Of[A, Option[B]] =
          Functor[container.Schema].map(self)(_.optional)

  implicit val schemaWriterOps: SchemaOps.Writer[Schema.Writer.Of, Schema.Writer.Of, Base.Schema.Writer[Identity, ?, *]] =
    new SchemaOps.Writer[Schema.Writer.Of, Schema.Writer.Of, Base.Schema.Writer[Identity, ?, *]]:
      extension [A, B](self: Schema.Writer.Of[A, B])
        override def optional: Schema.Writer.Of[A, Option[B]] =
          Functor[container.Schema].map(self)(_.optional)

  implicit def schemaReaderCoproductLiftOps: CoproductLiftOps[Schema.Reader.Of, Schema.Reader.Of, Union.Reader.Of] = ???

  implicit def schemaWriterCoproductLiftOps: CoproductLiftOps[Schema.Writer.Of, Schema.Writer.Of, Union.Writer.Of] = ???

trait Syntax5 extends Instances:
  object SchemaOps:
    trait Isomorphic[Self[a, b] <: Schema.Of[a, b], Optional[_, _], Plain[a] <: Base.Schema[Identity, ?, a]]
        extends Base.SchemaOps.Isomorphic[Self, Optional, Collection.Of, Union.Of, Plain]:
      extension [A, B](self: Self[A, B])
        final override def collection: Collection.Of[self.type, Vector[B]] =
          Applicative[container.Collection].pure(Base.Collection.Root(self))
        final override def union: Union.Of[self.type, B] =
          Applicative[container.Union].pure(Base.Union.Root(self))
        // override def toPlain: Base.Schema[Identity, ?, B] =
        //   Comonad[container.Schema]
        //     .extract(self)
        //     .translate([A] => (schema: container.Schema[A]) => Comonad[container.Schema].extract(schema))

    trait Reader[Self[a, b] <: Schema.Reader.Of[a, b], Optional[_, _], Plain[a] <: Base.Schema.Reader[Identity, ?, a]]
        extends Base.SchemaOps.Reader[Self, Optional, Collection.Reader.Of, Union.Reader.Of, Plain]:
      extension [A, B](self: Self[A, B])
        final override def collection: Collection.Reader.Of[self.type, Vector[B]] =
          Applicative[container.Collection].pure(Base.Collection.Reader.Root(self))
        final override def union: Union.Reader.Of[self.type, B] =
          Applicative[container.Union].pure(Base.Union.Reader.Root(self))
        // override def toPlain: Base.Schema.Reader[Identity, ?, B] = Comonad[container.Schema]
        //   .extract(self)
        //   .translate([A] => (schema: container.Schema[A]) => Comonad[container.Schema].extract(schema))

    trait Writer[Self[a, b] <: Schema.Writer.Of[a, b], Optional[_, _], Plain[a] <: Base.Schema.Writer[Identity, ?, a]]
        extends Base.SchemaOps.Writer[Self, Optional, Collection.Writer.Of, Union.Writer.Of, Plain]:
      extension [A, B](self: Self[A, B])
        final override def collection: Collection.Writer.Of[self.type, Vector[B]] =
          Applicative[container.Collection].pure(Base.Collection.Writer.Root(self))
        final override def union: Union.Writer.Of[self.type, B] =
          Applicative[container.Union].pure(Base.Union.Writer.Root(self))
        // override def toPlain: Base.Schema.Writer[Identity, ?, B] = Comonad[container.Schema]
        //   .extract(self)
        //   .translate([A] => (schema: container.Schema[A]) => Comonad[container.Schema].extract(schema))

  trait PrimitiveOps[Self[_], Plain[_]] extends Base.PrimitiveOps[Self, ?, ?, ?, Plain]:
    // extension [A, B](self: Self[B]) override def toPlain: Plain[B] =
    //   val x = Comonad[container.Primitive].extract(self)
    //   ???
    extension [A](self: Self[A]) override final def tpe: Base.Type[?] = Comonad[container.Primitive].extract(self).tpe

  object PrimitiveOps:
    trait Isomorphic[Self[a] <: Primitive[a], Optional[_], Plain[a] <: Base.Primitive[a]]
        extends Base.PrimitiveOps.Isomorphic[Self, Optional, Collection.Of, Union.Of, Plain],
          SchemaOps.Isomorphic[[_, a] =>> Self[a], [_, a] =>> Optional[a], Plain]:
      extension [A](self: Self[A]) override def tpe: Type[?] = Comonad[container.Primitive].extract(self).tpe
    trait Reader[Self[a] <: Primitive.Reader[a], Optional[_], Plain[a] <: Base.Primitive.Reader[a]]
        extends Base.PrimitiveOps.Reader[Self, Optional, Collection.Reader.Of, Union.Reader.Of, Plain],
          SchemaOps.Reader[[_, a] =>> Self[a], [_, a] =>> Optional[a], Plain]:
      extension [A](self: Self[A]) override def tpe: Type[?] = Comonad[container.Primitive].extract(self).tpe
    trait Writer[Self[a] <: Primitive.Writer[a], Optional[a], Plain[a] <: Base.Primitive.Writer[a]]
        extends Base.PrimitiveOps.Writer[Self, Optional, Collection.Writer.Of, Union.Writer.Of, Plain],
          SchemaOps.Writer[[_, a] =>> Self[a], [_, a] =>> Optional[a], Plain]:
      extension [A](self: Self[A]) override def tpe: Type[?] = Comonad[container.Primitive].extract(self).tpe

  object UnionOps:
    trait Isomorphic
        extends Base.UnionOps.Isomorphic[Union.Of, Schema.Of, Collection.Of],
          SchemaOps.Isomorphic[Union.Of, Union.Of, Base.Union[Identity, ?, *]]
    trait Reader
        extends Base.UnionOps.Reader[Union.Reader.Of, Schema.Reader.Of, Collection.Reader.Of],
          SchemaOps.Reader[Union.Reader.Of, Union.Reader.Of, Base.Union.Reader[Identity, ?, *]]
    trait Writer
        extends Base.UnionOps.Writer[Union.Writer.Of, Schema.Writer.Of, Collection.Writer.Of],
          SchemaOps.Writer[Union.Writer.Of, Union.Writer.Of, Base.Union.Writer[Identity, ?, *]]

private def toFunctorOps[Self[a, b] <: Reader[a, b], Reader[_, _], A, B](using
    F: Functor[Reader[A, *]]
): Conversion[Self[A, B], Functor.Ops[Reader[A, *], B]] = schema =>
  new Functor.Ops[Reader[A, *], B]:
    override type TypeClassType = Functor[Reader[A, *]]
    override def self: Self[A, B] = schema
    override val typeClassInstance: TypeClassType = F

private def toContravariantOps[Self[a, b] <: Writer[a, b], Writer[_, _], A, B](using
    F: Contravariant[Writer[A, *]]
): Conversion[Self[A, B], Contravariant.Ops[Writer[A, *], B]] = schema =>
  new Contravariant.Ops[Writer[A, *], B]:
    override type TypeClassType = Contravariant[Writer[A, *]]
    override def self: Self[A, B] = schema
    override val typeClassInstance: TypeClassType = F
