package io.taig.otter

import io.taig.otter as Base
import cats.Invariant
import cats.Functor
import cats.Contravariant
import cats.Comonad

trait Instances extends Types:
  given schemaInvariant[A](using F: Comonad[container.Schema]): Invariant[Schema.Of[A, *]] with
    override def imap[B, C](fa: Schema.Of[A, B])(f: B => C)(g: C => B): Schema.Of[A, C] =
      F.map(fa)(_.imap(f)(g))

  given schemaReaderFunctor[A](using F: Comonad[container.Schema]): Functor[Schema.Reader.Of[A, *]] with
    override def map[B, C](fa: Schema.Reader.Of[A, B])(f: B => C): Schema.Reader.Of[A, C] =
      F.map(fa)(_.map(f))

  given schemaWriterContravariant[A](using F: Comonad[container.Schema]): Contravariant[Schema.Writer.Of[A, *]] with
    override def contramap[B, C](fa: Schema.Writer.Of[A, B])(f: C => B): Schema.Writer.Of[A, C] =
      F.map(fa)(_.contramap(f))

  given primitiveRequiredIsomorphicInvariant(using
      F: Comonad[container.Primitive]
  ): ValidationInvariant.Primitive[Primitive.Required] with
    extension [A](self: Primitive.Required[A])
      override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
          f: D => A
      ): Primitive.Required[D] =
        F.map(self)(_.ivalidate(validation)(f))

  given primitiveRequiredReaderFunctor(using
      F: Comonad[container.Primitive]
  ): ValidationFunctor.Primitive[Primitive.Required.Reader] with
    extension [A](self: Primitive.Required.Reader[A])
      override def validate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D]): Primitive.Required.Reader[D] =
        F.map(self)(_.validate(validation))

  given primitiveRequiredWriterContravariant(using
      F: Comonad[container.Primitive]
  ): ValidationContravariant.Primitive[Primitive.Required.Writer] with
    override def contramap[A, B](fa: Primitive.Required.Writer[A])(f: B => A): Primitive.Required.Writer[B] =
      F.map(fa)(_.contramap(f))

  given primitiveIsomorphicInvariant(using F: Comonad[container.Primitive]): ValidationInvariant.Primitive[Primitive]
  with
    extension [A](self: Primitive[A])
      override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(f: D => A): Primitive[D] =
        F.map(self)(_.ivalidate(validation)(f))

  given primitiveReaderFunctor(using F: Comonad[container.Primitive]): ValidationFunctor.Primitive[Primitive.Reader]
  with
    extension [A](self: Primitive.Reader[A])
      override def validate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D]): Primitive.Reader[D] =
        F.map(self)(_.validate(validation))

  given primitiveWriterContravariant(using
      F: Comonad[container.Primitive]
  ): ValidationContravariant.Primitive[Primitive.Writer] with
    override def contramap[A, B](fa: Primitive.Writer[A])(f: B => A): Primitive.Writer[B] =
      F.map(fa)(_.contramap(f))
