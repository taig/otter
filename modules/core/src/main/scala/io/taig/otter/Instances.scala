package io.taig.otter

import io.taig.otter as Base
import cats.data.Chain
import io.taig.otter.validation.Constraint

trait Instances extends Types:
  given schemaInvariant[A]: SchemaInvariant[Schema.Of[A, *]] with
    override def constraints[B](fa: Schema.Of[A, B]): Chain[Constraint[?]] = ???

    override def ivalidate[B, V1, V2, C](fa: Schema.Of[A, B])(validation: Validation[B, V1, V2, C])(
        f: C => B
    ): Schema.Of[A, C] = ???

  given schemaReaderFunctor[A]: SchemaFunctor[Schema.Reader.Of[A, *]] with
    override def constraints[B](fa: Schema.Reader.Of[A, B]): Chain[Constraint[?]] = ???
    override def validate[B, V1, V2, C](fa: Schema.Reader.Of[A, B])(
        validation: Validation[B, V1, V2, C]
    ): Schema.Reader.Of[A, C] = ???

  given schemaWriterContravariant[A]: SchemaContravariant[Schema.Writer.Of[A, *]] with
    override def contramap[B, C](fa: Schema.Writer.Of[A, B])(f: C => B): Schema.Writer.Of[A, C] = fa.contramap(f)

  given primitiveInvariant: SchemaInvariant[Primitive] with
    override def constraints[A](fa: Primitive[A]): Chain[Constraint[?]] = ???
    override def ivalidate[A, V1, V2, B](fa: Primitive[A])(validation: Validation[A, V1, V2, B])(
        f: B => A
    ): Primitive[B] = ???

  given primitiveRequiredInvariant: SchemaInvariant[Primitive.Required] with
    override def constraints[A](fa: Primitive.Required[A]): Chain[Constraint[?]] = ???
    override def ivalidate[A, V1, V2, B](fa: Primitive.Required[A])(validation: Validation[A, V1, V2, B])(
        f: B => A
    ): Primitive.Required[B] = ???

  given primitiveReaderFunctor: SchemaFunctor[Primitive.Reader] with
    override def constraints[A](fa: Primitive.Reader[A]): Chain[Constraint[?]] = ???
    override def validate[A, V1, V2, B](fa: Primitive.Reader[A])(
        validation: Validation[A, V1, V2, B]
    ): Primitive.Reader[B] = ???

  given primitiveRequiredReaderFunctor: SchemaFunctor[Primitive.Required.Reader] with
    override def constraints[A](fa: Primitive.Required.Reader[A]): Chain[Constraint[?]] = ???
    override def validate[A, V1, V2, B](fa: Primitive.Required.Reader[A])(
        validation: Validation[A, V1, V2, B]
    ): Primitive.Required.Reader[B] = ???

  given primitiveWriterContravariant: SchemaContravariant[Primitive.Writer] with
    override def contramap[A, B](fa: Primitive.Writer[A])(f: B => A): Primitive.Writer[B] = fa.contramap(f)

  given primitiveRequiredWriterContravariant: SchemaContravariant[Primitive.Required.Writer] with
    override def contramap[A, B](fa: Primitive.Required.Writer[A])(f: B => A): Primitive.Required.Writer[B] =
      fa.contramap(f)
