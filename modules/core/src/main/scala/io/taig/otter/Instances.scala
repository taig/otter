package io.taig.otter

import io.taig.otter as Base
import cats.Invariant
import cats.Functor
import cats.Contravariant

trait Instances extends Instances1:
  implicit def collectionInvariant[A]: ValidationInvariant.Collection[Collection.Of[A, *]] =
    new ValidationInvariant.Collection[Collection.Of[A, *]]:
      extension [B](fa: Collection.Of[A, B])
        override def ivalidate[C, D, E](validation: SchemaValidation.Collection[B, D, E])(
            g: E => B
        ): Collection.Of[A, E] = Functor[container.Collection].map(fa)(_.ivalidate(validation)(g))

  implicit def primitiveRequiredInvariant: ValidationInvariant.Primitive[Primitive.Required] =
    new ValidationInvariant.Primitive[Primitive.Required]:
      extension [A](fa: Primitive.Required[A])
        override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
            g: D => A
        ): Primitive.Required[D] = Functor[container.Primitive].map(fa)(_.ivalidate(validation)(g))

  implicit def unionInvariant[A]: Invariant[Union.Of[A, *]] = ???

  implicit def unionFunctor[A]: Functor[Union.Reader.Of[A, *]] = ???

  implicit def unionContravariant[A]: Contravariant[Union.Writer.Of[A, *]] = ???

trait Instances1 extends Instances2:
  implicit def collectionFunctor[A]: ValidationFunctor.Collection[Collection.Reader.Of[A, *]] = ???

  implicit def collectionContravariant[A]: ValidationContravariant.Collection[Collection.Writer.Of[A, *]] = ???

  implicit def primitiveRequiredFunctor: ValidationFunctor.Primitive[Primitive.Required.Reader] = ???

  implicit def primitiveRequiredContravariant: ValidationContravariant.Primitive[Primitive.Required.Writer] = ???

trait Instances2 extends Instances3:
  implicit def primitiveIsomorphicInvariant: ValidationInvariant.Primitive[Primitive] = ???

  implicit def primitiveFunctor: ValidationFunctor.Primitive[Primitive.Reader] = ???

  implicit def primitiveContravariant: ValidationContravariant.Primitive[Primitive.Writer] = ???

  implicit def schemaIsomporhicInvariant[A]: Invariant[Schema.Of[A, *]] = new Invariant[Schema.Of[A, *]]:
    override def imap[B, C](fa: Schema.Of[A, B])(f: B => C)(g: C => B): Schema.Of[A, C] =
      Functor[container.Schema].map(fa)(_.imap(f)(g))

  implicit def schemaFunctor[A]: Functor[Schema.Reader.Of[A, *]] = new Functor[Schema.Reader.Of[A, *]]:
    override def map[B, C](fa: Schema.Reader.Of[A, B])(f: B => C): Schema.Reader.Of[A, C] =
      Functor[container.Schema].map(fa)(_.map(f))

  implicit def schemaContravariant[A]: Contravariant[Schema.Writer.Of[A, *]] =
    new Contravariant[Schema.Writer.Of[A, *]]:
      override def contramap[B, C](fa: Schema.Writer.Of[A, B])(f: C => B): Schema.Writer.Of[A, C] =
        Functor[container.Schema].map(fa)(_.contramap(f))

trait Instances3 extends Types:
  implicit def schemaApplicativeComonad: ApplicativeComonad[container.Schema]
  implicit def collectionApplicativeComonad: ApplicativeComonad[container.Collection]
  implicit def primitiveApplicativeComonad: ApplicativeComonad[container.Primitive]
  implicit def unionApplicativeComonad: ApplicativeComonad[container.Union]
