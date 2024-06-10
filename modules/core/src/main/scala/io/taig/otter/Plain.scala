package io.taig.otter

import io.taig.otter as Base
import cats.data.Chain

object Plain extends Dsl:
  override type AsSchema[+A] = A
  override type AsCollection[+A] = A
  override type AsPrimitive[+A] = A
  override type AsTuple[+A] = A
  override type AsUnion[+A] = A

  override def primitive[A](tpe: Base.Type[A]): Primitive.Required[A] =
    Base.Primitive.Required.Root(tpe)

  override given schemaOps: SchemaOps[Schema.Of, Schema.Of, Collection.Of, Tuple.Of] with
    extension [A, B](self: Schema.Of[A, B])
      override def collection: Collection.Of[self.type, Vector[B]] = Base.Collection.Root(self)
      override def optional: Schema.Of[A, Option[B]] = self.optional
      override def toTuple: Tuple.Of[self.type, B] = Base.Tuple.One(self)

  override given schemaInvariant[A]: SchemaInvariant[Schema.Of[A, *]] = new SchemaInvariant[Schema.Of[A, *]]:
    extension [B](self: Schema.Of[A, B])
      override def constraints: Chain[Base.validation.Constraint[?]] = self.constraints

      override def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Schema.Of[A, C] =
        self.ivalidate(validation)(f)

  override given schemaReaderOps: SchemaOps[Schema.Reader.Of, Schema.Reader.Of, Collection.Reader.Of, Tuple.Reader.Of]
  with
    extension [A, B](self: Schema.Reader.Of[A, B])
      override def collection: Collection.Reader.Of[self.type, Vector[B]] = Base.Collection.Reader.Root(self)
      override def optional: Schema.Reader.Of[A, Option[B]] = self.optional
      override def toTuple: Tuple.Reader.Of[self.type, B] = Base.Tuple.Reader.One(self)

  override given schemaReaderFunctor[A]: SchemaFunctor[Schema.Reader.Of[A, *]] =
    new SchemaFunctor[Schema.Reader.Of[A, *]]:
      extension [B](self: Schema.Reader.Of[A, B])
        override def constraints: Chain[Base.validation.Constraint[?]] = self.constraints
        override def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Schema.Reader.Of[A, C] =
          self.validate(validation)

  override given schemaWriterOps: SchemaOps[Schema.Writer.Of, Schema.Writer.Of, Collection.Writer.Of, Tuple.Writer.Of]
  with
    extension [A, B](self: Schema.Writer.Of[A, B])
      override def collection: Collection.Writer.Of[self.type, Vector[B]] = Base.Collection.Writer.Root(self)
      override def optional: Schema.Writer.Of[A, Option[B]] = self.optional
      override def toTuple: Tuple.Writer.Of[self.type, B] = Base.Tuple.Writer.One(self)

  override given schemaWriterContravariant[A]: SchemaContravariant[Schema.Writer.Of[A, *]] =
    new SchemaContravariant[Schema.Writer.Of[A, *]]:
      override def contramap[B, C](fa: Schema.Writer.Of[A, B])(f: C => B): Schema.Writer.Of[A, C] = fa.contramap(f)

  override given collectionOps: CollectionOps[Collection.Of, Tuple.Of, Schema.Any] with
    extension [A, B](self: Collection.Of[A, B])
      override def collection: Collection.Of[self.type, Vector[B]] = Base.Collection.Root(self)
      override def optional: Collection.Of[A, Option[B]] = self.optional
      override def toTuple: Tuple.Of[self.type, B] = Base.Tuple.One(self)
      override def schema: Schema.Any = self.schema

  override given collectionInvariant[A]: SchemaInvariant[Collection.Of[A, *]] =
    new SchemaInvariant[Collection.Of[A, *]]:
      extension [B](self: Collection.Of[A, B])
        override def constraints: Chain[Base.validation.Constraint[?]] = self.constraints
        override def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Collection.Of[A, C] =
          self.ivalidate(validation)(f)

  override given collectionReaderOps: CollectionOps[Collection.Reader.Of, Tuple.Reader.Of, Schema.Reader.Any] with
    extension [A, B](self: Collection.Reader.Of[A, B])
      override def collection: Collection.Reader.Of[self.type, Vector[B]] = Base.Collection.Reader.Root(self)
      override def optional: Collection.Reader.Of[A, Option[B]] = self.optional
      override def toTuple: Tuple.Reader.Of[self.type, B] = Base.Tuple.Reader.One(self)
      override def schema: Schema.Reader.Any = self.schema

  override given collectionReaderFunctor[A]: SchemaFunctor[Collection.Reader.Of[A, *]] =
    new SchemaFunctor[Collection.Reader.Of[A, *]]:
      extension [B](self: Collection.Reader.Of[A, B])
        override def constraints: Chain[Base.validation.Constraint[?]] = self.constraints
        override def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Collection.Reader.Of[A, C] =
          self.validate(validation)

  override given collectionWriterOps: CollectionOps[Collection.Writer.Of, Tuple.Writer.Of, Schema.Writer.Any] =
    new CollectionOps[Collection.Writer.Of, Tuple.Writer.Of, Schema.Writer.Any]:
      extension [A, B](self: Collection.Writer.Of[A, B])
        override def collection: Collection.Writer.Of[self.type, Vector[B]] = Base.Collection.Writer.Root(self)
        override def optional: Collection.Writer.Of[A, Option[B]] = self.optional
        override def toTuple: Tuple.Writer.Of[self.type, B] = Base.Tuple.Writer.One(self)
        override def schema: Schema.Writer.Any = self.schema

  override given collectionWriterContravariant[A]: SchemaContravariant[Collection.Writer.Of[A, *]] =
    new SchemaContravariant[Collection.Writer.Of[A, *]]:
      override def contramap[B, C](fa: Collection.Writer.Of[A, B])(f: C => B): Collection.Writer.Of[A, C] =
        fa.contramap(f)

  override given primitiveOps[F[a] <: Primitive[a]]: PrimitiveOps[F, Primitive, Collection.Of, Tuple.Of] =
    new PrimitiveOps[F, Primitive, Collection.Of, Tuple.Of]:
      extension [A, B](self: F[B])
        override def collection: Collection.Of[self.type, Vector[B]] = Base.Collection.Root(self)
        override def optional: Primitive[Option[B]] = self.optional
        override def toTuple: Tuple.Of[self.type, B] = Base.Tuple.One(self)

      extension [A](self: F[A]) override def tpe: Base.Type[?] = self.tpe

  override given primitiveInvariant[A]: SchemaInvariant[Primitive] = new SchemaInvariant[Primitive]:

    extension [A](self: Primitive[A])
      override def constraints: Chain[Base.validation.Constraint[?]] = self.constraints
      override def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): Primitive[B] =
        self.ivalidate(validation)(f)

  override given primitiveReaderOps[F[a] <: Primitive.Reader[a]]
      : PrimitiveOps[F, Primitive.Reader, Collection.Reader.Of, Tuple.Reader.Of] =
    new PrimitiveOps[F, Primitive.Reader, Collection.Reader.Of, Tuple.Reader.Of]:
      extension [A, B](self: F[B])
        override def collection: Collection.Reader.Of[self.type, Vector[B]] = Base.Collection.Reader.Root(self)
        override def optional: Primitive.Reader[Option[B]] = self.optional
        override def toTuple: Tuple.Reader.Of[self.type, B] = Base.Tuple.Reader.One(self)

      extension [A](self: F[A]) override def tpe: Base.Type[?] = self.tpe

  override given primitiveReaderFunctor[A]: SchemaFunctor[Primitive.Reader] = new SchemaFunctor[Primitive.Reader]:
    extension [A](self: Primitive.Reader[A])
      override def constraints: Chain[Base.validation.Constraint[?]] = self.constraints
      override def validate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B]): Primitive.Reader[B] =
        self.validate(validation)

  override given primitiveWriterOps[F[a] <: Primitive.Writer[a]]
      : PrimitiveOps[F, Primitive.Writer, Collection.Writer.Of, Tuple.Writer.Of] =
    new PrimitiveOps[F, Primitive.Writer, Collection.Writer.Of, Tuple.Writer.Of]:
      extension [A, B](self: F[B])
        override def collection: Collection.Writer.Of[self.type, Vector[B]] = Base.Collection.Writer.Root(self)
        override def optional: Primitive.Writer[Option[B]] = self.optional
        override def toTuple: Tuple.Writer.Of[self.type, B] = Base.Tuple.Writer.One(self)

      extension [A](self: F[A]) override def tpe: Base.Type[?] = self.tpe

  override given primitiveWriterContravariant[A]: SchemaContravariant[Primitive.Writer] =
    new SchemaContravariant[Primitive.Writer]:
      override def contramap[A, B](fa: Primitive.Writer[A])(f: B => A): Primitive.Writer[B] = fa.contramap(f)
