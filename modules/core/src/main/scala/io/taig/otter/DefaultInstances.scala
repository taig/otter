package io.taig.otter

import io.taig.otter as Base
import cats.data.Chain
import io.taig.otter.validation.Constraint

trait DefaultInstances extends Instances, Schemas:
  given asSchema: ApplicativeComonad[AsSchema]
  given asCollection: ApplicativeComonad[AsCollection]
  given asTuple: ApplicativeComonad[AsTuple]
  given asPrimitive: ApplicativeComonad[AsPrimitive]

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    asPrimitive.pure(Base.Primitive.Required.Root(tpe))

  override given schemaOps: SchemaOps[Schema.Of, Schema.Of, Collection.Of, Tuple.Of] with
    extension [A, B](self: Schema.Of[A, B])
      override def collection: Collection.Of[self.type, Vector[B]] =
        asCollection.pure(Base.Collection.Root(self))
      override def optional: Schema.Of[A, Option[B]] = asSchema.map(self)(_.optional)
      override def tuple: Tuple.Of[self.type, B] = asTuple.pure(Base.Tuple.One(self))

  override given schemaInvariant[A]: SchemaInvariant[Schema.Of[A, *]] = new SchemaInvariant[Schema.Of[A, *]]:
    extension [B](self: Schema.Of[A, B])
      override def constraints: Chain[Constraint[?]] = asSchema.extract(self).constraints
      override def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Schema.Of[A, C] =
        asSchema.map(self)(_.ivalidate(validation)(f))

  override given schemaReaderOps: SchemaOps[Schema.Reader.Of, Schema.Reader.Of, Collection.Reader.Of, Tuple.Reader.Of]
  with
    extension [A, B](self: Schema.Reader.Of[A, B])
      override def collection: Collection.Reader.Of[self.type, Vector[B]] =
        asCollection.pure(Base.Collection.Reader.Root(self))
      override def optional: Schema.Reader.Of[A, Option[B]] = asSchema.map(self)(_.optional)
      override def tuple: Tuple.Reader.Of[self.type, B] = asTuple.pure(Base.Tuple.Reader.One(self))

  override given schemaReaderFunctor[A]: SchemaFunctor[Schema.Reader.Of[A, *]] = new SchemaFunctor:
    extension [B](self: Schema.Reader.Of[A, B])
      override def constraints: Chain[Constraint[?]] = asSchema.extract(self).constraints
      override def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Schema.Reader.Of[A, C] =
        asSchema.map(self)(_.validate(validation))

  override given schemaWriterOps: SchemaOps[Schema.Writer.Of, Schema.Writer.Of, Collection.Writer.Of, Tuple.Writer.Of]
  with
    extension [A, B](self: Schema.Writer.Of[A, B])
      override def collection: Collection.Writer.Of[self.type, Vector[B]] =
        asCollection.pure(Base.Collection.Writer.Root(self))
      override def optional: Schema.Writer.Of[A, Option[B]] = asSchema.map(self)(_.optional)
      override def tuple: Tuple.Writer.Of[self.type, B] = asTuple.pure(Base.Tuple.Writer.One(self))

  override given schemaWriterContravariant[A]: SchemaContravariant[Schema.Writer.Of[A, *]] = new SchemaContravariant:
    override def contramap[B, C](fa: Schema.Writer.Of[A, B])(f: C => B): Schema.Writer.Of[A, C] =
      asSchema.map(fa)(_.contramap(f))

  override given collectionOps: CollectionOps[Collection.Of, Tuple.Of, Schema.Any] with
    extension [A, B](self: Collection.Of[A, B])
      override def collection: Collection.Of[self.type, Vector[B]] = asCollection.pure(Base.Collection.Root(self))
      override def optional: Collection.Of[A, Option[B]] = asCollection.map(self)(_.optional)
      override def tuple: Tuple.Of[self.type, B] = asTuple.pure(Base.Tuple.One(self))
      override def schema: Schema.Any = asCollection.extract(self).schema

  override given collectionInvariant[A]: SchemaInvariant[Collection.Of[A, *]] = new SchemaInvariant:
    extension [B](self: Collection.Of[A, B])
      override def constraints: Chain[Constraint[?]] = asCollection.extract(self).constraints
      override def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Collection.Of[A, C] =
        asCollection.map(self)(_.ivalidate(validation)(f))

  override given collectionReaderOps: CollectionOps[Collection.Reader.Of, Tuple.Reader.Of, Schema.Reader.Any] with
    extension [A, B](self: Collection.Reader.Of[A, B])
      override def collection: Collection.Reader.Of[self.type, Vector[B]] =
        asCollection.pure(Base.Collection.Reader.Root(self))
      override def optional: Collection.Reader.Of[A, Option[B]] = asCollection.map(self)(_.optional)
      override def tuple: Tuple.Reader.Of[self.type, B] = asTuple.pure(Base.Tuple.Reader.One(self))
      override def schema: Schema.Reader.Any = asCollection.extract(self).schema

  override given collectionReaderFunctor[A]: SchemaFunctor[Collection.Reader.Of[A, *]] = new SchemaFunctor:
    extension [B](self: Collection.Reader.Of[A, B])
      override def constraints: Chain[Constraint[?]] = asCollection.extract(self).constraints
      override def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Collection.Reader.Of[A, C] =
        asCollection.map(self)(_.validate(validation))

  override given collectionWriterOps: CollectionOps[Collection.Writer.Of, Tuple.Writer.Of, Schema.Writer.Any] with
    extension [A, B](self: Collection.Writer.Of[A, B])
      override def collection: Collection.Writer.Of[self.type, Vector[B]] =
        asCollection.pure(Base.Collection.Writer.Root(self))
      override def optional: Collection.Writer.Of[A, Option[B]] = asCollection.map(self)(_.optional)
      override def tuple: Tuple.Writer.Of[self.type, B] = asTuple.pure(Base.Tuple.Writer.One(self))
      override def schema: Schema.Writer.Any = asCollection.extract(self).schema

  override given collectionWriterContravariant[A]: SchemaContravariant[Collection.Writer.Of[A, *]] =
    new SchemaContravariant:
      override def contramap[B, C](fa: Collection.Writer.Of[A, B])(f: C => B): Collection.Writer.Of[A, C] =
        asCollection.map(fa)(_.contramap(f))

  override given primitiveOps: PrimitiveOps[Primitive, Primitive, Collection.Of, Tuple.Of] with
    extension [A, B](self: Primitive[B])
      override def collection: Collection.Of[self.type, Vector[B]] = asCollection.pure(Base.Collection.Root(self))
      override def optional: Primitive[Option[B]] = asPrimitive.map(self)(_.optional)
      override def tuple: Tuple.Of[self.type, B] = asTuple.pure(Base.Tuple.One(self))

    extension [A](self: Primitive[A]) override def tpe: Type[?] = asPrimitive.extract(self).tpe

  override given primitiveInvariant: SchemaInvariant[Primitive] with
    extension [A](self: Primitive[A])
      override def constraints: Chain[Constraint[?]] = asPrimitive.extract(self).constraints
      override def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): Primitive[B] =
        asPrimitive.map(self)(_.ivalidate(validation)(f))

  override given primitiveReaderOps
      : PrimitiveOps[Primitive.Reader, Primitive.Reader, Collection.Reader.Of, Tuple.Reader.Of] with
    extension [A, B](self: Primitive.Reader[B])
      override def collection: Collection.Reader.Of[self.type, Vector[B]] =
        asCollection.pure(Base.Collection.Reader.Root(self))
      override def optional: Primitive.Reader[Option[B]] = asPrimitive.map(self)(_.optional)
      override def tuple: Tuple.Reader.Of[self.type, B] = asTuple.pure(Base.Tuple.Reader.One(self))

    extension [A](self: Primitive.Reader[A]) override def tpe: Type[?] = asPrimitive.extract(self).tpe

  override given primitiveReaderFunctor: SchemaFunctor[Primitive.Reader] with
    extension [A](self: Primitive.Reader[A])
      override def constraints: Chain[Constraint[?]] = asPrimitive.extract(self).constraints
      override def validate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B]): Primitive.Reader[B] =
        asPrimitive.map(self)(_.validate(validation))

  override given primitiveWriterOps
      : PrimitiveOps[Primitive.Writer, Primitive.Writer, Collection.Writer.Of, Tuple.Writer.Of] with
    extension [A, B](self: Primitive.Writer[B])
      override def collection: Collection.Writer.Of[self.type, Vector[B]] =
        asCollection.pure(Base.Collection.Writer.Root(self))
      override def optional: Primitive.Writer[Option[B]] = asPrimitive.map(self)(_.optional)
      override def tuple: Tuple.Writer.Of[self.type, B] = asTuple.pure(Base.Tuple.Writer.One(self))

    extension [A](self: Primitive.Writer[A]) override def tpe: Type[?] = asPrimitive.extract(self).tpe

  override given primitiveWriterContravariant: SchemaContravariant[Primitive.Writer] with
    override def contramap[A, B](fa: Primitive.Writer[A])(f: B => A): Primitive.Writer[B] =
      asPrimitive.map(fa)(_.contramap(f))

  override given primitiveRequiredInvariant: SchemaInvariant[Primitive.Required] with
    extension [A](self: Primitive.Required[A])
      override def constraints: Chain[Constraint[?]] = asPrimitive.extract(self).constraints
      override def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): Primitive.Required[B] =
        ???

  override given primitiveRequiredOps: PrimitiveOps[Primitive.Required, Primitive, Collection.Of, Tuple.Of] with
    extension [A, B](self: Primitive.Required[B])
      override def collection: Collection.Of[self.type, Vector[B]] = asCollection.pure(Base.Collection.Root(self))

      override def optional: Primitive[Option[B]] = asPrimitive.map(self)(_.optional)

      override def tuple: Tuple.Of[self.type, B] = asTuple.pure(Base.Tuple.One(self))

    extension [A](self: Primitive.Required[A]) override def tpe: Type[?] = ???

  override given primitiveRequiredReaderFunctor: SchemaFunctor[Primitive.Required.Reader] with

    extension [A](self: Primitive.Required.Reader[A])
      override def constraints: Chain[Constraint[?]] = asPrimitive.extract(self).constraints

      override def validate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B]): Primitive.Required.Reader[B] = ???

  override given primitiveRequiredReaderOps: PrimitiveOps[
    Primitive.Required.Reader,
    Primitive.Reader,
    Collection.Reader.Of,
    Tuple.Reader.Of
  ] with
    extension [A, B](self: Primitive.Required.Reader[B])
      override def collection: Collection.Reader.Of[self.type, Vector[B]] =
        asCollection.pure(Base.Collection.Reader.Root(self))

      override def optional: Primitive.Reader[Option[B]] = asPrimitive.map(self)(_.optional)

      override def tuple: Tuple.Reader.Of[self.type, B] = asTuple.pure(Base.Tuple.Reader.One(self))

    extension [A](self: Primitive.Required.Reader[A]) override def tpe: Type[?] = asPrimitive.extract(self).tpe

  override given primitiveRequiredWriterContravariant: SchemaContravariant[Primitive.Required.Writer] with

    override def contramap[A, B](fa: Primitive.Required.Writer[A])(f: B => A): Primitive.Required.Writer[B] = ???

  override given primitiveRequiredWriterOps
      : PrimitiveOps[Primitive.Required.Writer, Primitive.Writer, Collection.Writer.Of, Tuple.Writer.Of] with
    extension [A, B](self: Primitive.Required.Writer[B])
      override def collection: Collection.Writer.Of[self.type, Vector[B]] =
        asCollection.pure(Base.Collection.Writer.Root(self))

      override def optional: Primitive.Writer[Option[B]] = ???

      override def tuple: Tuple.Writer.Of[self.type, B] = ???

    extension [A](self: Primitive.Required.Writer[A]) override def tpe: Type[?] = asPrimitive.extract(self).tpe
