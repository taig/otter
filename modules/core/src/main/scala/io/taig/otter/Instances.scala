package io.taig.otter

import io.taig.otter as Base
import cats.Invariant
import cats.Functor
import cats.Contravariant
import io.taig.otter.validation.Validation

trait Instances extends Types
// trait Instances extends Instances1:
//   implicit def collectionInvariant[A]: ValidationInvariant.Collection[Collection.Of[A, *]] =
//     new ValidationInvariant.Collection[Collection.Of[A, *]]:
//       extension [B](fa: Collection.Of[A, B])
//         override def ivalidate[C, D, E](validation: SchemaValidation.Collection[B, D, E])(
//             g: E => B
//         ): Collection.Of[A, E] = Functor[container.Collection].map(fa)(_.ivalidate(validation)(g))

//   implicit val primitiveRequiredInvariant: ValidationInvariant.Primitive[Primitive.Required] =
//     new ValidationInvariant.Primitive[Primitive.Required]:
//       extension [A](fa: Primitive.Required[A])
//         override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
//             g: D => A
//         ): Primitive.Required[D] = Functor[container.Primitive].map(fa)(_.ivalidate(validation)(g))

//   implicit def unionInvariant[A]: Invariant[Union.Of[A, *]] = new Invariant[Union.Of[A, *]]:
//     override def imap[B, C](fa: Union.Of[A, B])(f: B => C)(g: C => B): Union.Of[A, C] =
//       Functor[container.Union].map(fa)(_.imap(f)(g))

// trait Instances1 extends Instances2:
//   implicit def collectionFunctor[X]: ValidationFunctor.Collection[Collection.Reader.Of[X, *]] =
//     new ValidationFunctor.Collection[Collection.Reader.Of[X, *]]:
//       extension [A](fa: Collection.Reader.Of[X, A])
//         override def validate[B, C, D](
//             validation: Validation[A, Constraint.Collection, ValidationWriter[C], D]
//         ): Collection.Reader.Of[X, D] =
//           Functor[container.Collection].map(fa)(_.validate(validation))

//   implicit def collectionContravariant[A]: ValidationContravariant.Collection[Collection.Writer.Of[A, *]] =
//     new ValidationContravariant.Collection[Collection.Writer.Of[A, *]]:
//       override def contramap[B, C](fa: Collection.Writer.Of[A, B])(f: C => B): Collection.Writer.Of[A, C] =
//         Functor[container.Collection].map(fa)(_.contramap(f))

//   implicit val primitiveRequiredFunctor: ValidationFunctor.Primitive[Primitive.Required.Reader] =
//     new ValidationFunctor.Primitive[Primitive.Required.Reader]:
//       extension [A](fa: Primitive.Required.Reader[A])
//         override def validate[B, C, D](
//             validation: SchemaValidation.Primitive[A, B, C, D]
//         ): Primitive.Required.Reader[D] = Functor[container.Primitive].map(fa)(_.validate(validation))

//   implicit val primitiveRequiredContravariant: ValidationContravariant.Primitive[Primitive.Required.Writer] =
//     new ValidationContravariant.Primitive[Primitive.Required.Writer]:
//       override def contramap[A, B](fa: Primitive.Required.Writer[A])(f: B => A): Primitive.Required.Writer[B] =
//         Functor[container.Primitive].map(fa)(_.contramap(f))

//   implicit def unionFunctor[A]: Functor[Union.Reader.Of[A, *]] = new Functor[Union.Reader.Of[A, *]]:
//     override def map[B, C](fa: Union.Reader.Of[A, B])(f: B => C): Union.Reader.Of[A, C] =
//       Functor[container.Union].map(fa)(_.map(f))

//   implicit def unionContravariant[A]: Contravariant[Union.Writer.Of[A, *]] = new Contravariant[Union.Writer.Of[A, *]]:
//     override def contramap[B, C](fa: Union.Writer.Of[A, B])(f: C => B): Union.Writer.Of[A, C] =
//       Functor[container.Union].map(fa)(_.contramap(f))

// trait Instances2 extends Instances3:
//   implicit val primitiveInvariant: ValidationInvariant.Primitive[Primitive] =
//     new ValidationInvariant.Primitive[Primitive]:
//       extension [A](fa: Primitive[A])
//         override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(f: D => A): Primitive[D] =
//           Functor[container.Primitive].map(fa)(_.ivalidate(validation)(f))

// trait Instances3 extends Instances4:
//   implicit val primitiveFunctor: ValidationFunctor.Primitive[Primitive.Reader] =
//     new ValidationFunctor.Primitive[Primitive.Reader]:
//       extension [A](fa: Primitive.Reader[A])
//         override def validate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D]): Primitive.Reader[D] =
//           Functor[container.Primitive].map(fa)(_.validate(validation))

//   implicit val primitiveContravariant: ValidationContravariant.Primitive[Primitive.Writer] =
//     new ValidationContravariant.Primitive[Primitive.Writer]:
//       override def contramap[A, B](fa: Primitive.Writer[A])(f: B => A): Primitive.Writer[B] =
//         Functor[container.Primitive].map(fa)(_.contramap(f))

// trait Instances4 extends Instances5:
//   implicit def schemaInvariant[A]: Invariant[Schema.Of[A, *]] = new Invariant[Schema.Of[A, *]]:
//     override def imap[B, C](fa: Schema.Of[A, B])(f: B => C)(g: C => B): Schema.Of[A, C] =
//       Functor[container.Schema].map(fa)(_.imap(f)(g))

// trait Instances5 extends Instances6:
//   implicit def schemaFunctor[A]: Functor[Schema.Reader.Of[A, *]] = new Functor[Schema.Reader.Of[A, *]]:
//     override def map[B, C](fa: Schema.Reader.Of[A, B])(f: B => C): Schema.Reader.Of[A, C] =
//       Functor[container.Schema].map(fa)(_.map(f))

//   implicit def schemaContravariant[A]: Contravariant[Schema.Writer.Of[A, *]] =
//     new Contravariant[Schema.Writer.Of[A, *]]:
//       override def contramap[B, C](fa: Schema.Writer.Of[A, B])(f: C => B): Schema.Writer.Of[A, C] =
//         Functor[container.Schema].map(fa)(_.contramap(f))

trait Instances6 extends Types
// implicit def schemaApplicativeComonad: ApplicativeComonad[container.Schema]
// implicit def collectionApplicativeComonad: ApplicativeComonad[container.Collection]
// implicit def primitiveApplicativeComonad: ApplicativeComonad[container.Primitive]
// implicit def unionApplicativeComonad: ApplicativeComonad[container.Union]
