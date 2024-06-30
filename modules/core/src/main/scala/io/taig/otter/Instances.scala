package io.taig.otter

import io.taig.otter as Base
import cats.Invariant
import cats.Functor
import cats.Contravariant

trait Instances extends Instances1:
  given collectionInvariant[A]: ValidationInvariant.Collection[Collection.Of[A, *]] with
    extension [B](fa: Collection.Of[A, B])
      override def ivalidate[C, D, E](validation: SchemaValidation.Collection[B, D, E])(
          g: E => B
      ): Collection.Of[A, E] =
        ???

  given primitiveRequiredInvariant: ValidationInvariant.Primitive[Primitive.Required] = ???

  given unionInvariant[A]: Invariant[Union.Of[A, *]] = ???

  given unionFunctor[A]: Functor[Union.Reader.Of[A, *]] = ???

  given unionContravariant[A]: Contravariant[Union.Writer.Of[A, *]] = ???

trait Instances1 extends Instances2:
  given collectionFunctor[A]: ValidationFunctor.Collection[Collection.Reader.Of[A, *]] = ???

  given collectionContravariant[A]: ValidationContravariant.Collection[Collection.Writer.Of[A, *]] = ???

  given primitiveRequiredFunctor: ValidationFunctor.Primitive[Primitive.Required.Reader] = ???

  given primitiveRequiredContravariant: ValidationContravariant.Primitive[Primitive.Required.Writer] = ???

trait Instances2 extends Types:
  given primitiveIsomorphicInvariant: ValidationInvariant.Primitive[Primitive] = ???

  given primitiveFunctor: ValidationFunctor.Primitive[Primitive.Reader] = ???

  given primitiveContravariant: ValidationContravariant.Primitive[Primitive.Writer] = ???

  given schemaIsomporhicInvariant[A]: Invariant[Schema.Of[A, *]] = ???
  // with
  //   override def imap[B, C](fa: Schema.Of[A, B])(f: B => C)(g: C => B): Schema.Of[A, C] = F.map(fa)(_.imap(f)(g))

  given schemaFunctor[A]: Functor[Schema.Reader.Of[A, *]] = ???
  // with
  //   override def map[B, C](fa: Schema.Reader.Of[A, B])(f: B => C): Schema.Reader.Of[A, C] = F.map(fa)(_.map(f))

  given schemaContravariant[A]: Contravariant[Schema.Writer.Of[A, *]] = ???
  // with
  //   override def contramap[B, C](fa: Schema.Writer.Of[A, B])(f: C => B): Schema.Writer.Of[A, C] =
  //     F.map(fa)(_.contramap(f))
