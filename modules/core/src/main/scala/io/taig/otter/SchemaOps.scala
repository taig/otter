package io.taig.otter

import cats.Functor
import cats.Invariant
import cats.Contravariant
import cats.syntax.all.*
import io.taig.otter.validation.Validation

// trait ValidationIsomorphicOps[Self[_, _], Constraint[_]: Functor, Writer[_]]:
//   extension [A, B](self: Self[A, B])
//     def ivalidate[C, D, E](validation: Validation[B, Constraint[(Writer[C], C)], (Writer[D], D), E])(
//         f: E => B
//     ): Self[A, E]

//     final def ivalidate[C, D, E](
//         validation: Validation[B, Constraint[C], D, E],
//         constraint: Writer[C],
//         actual: Writer[D]
//     )(
//         f: E => B
//     ): Self[A, E] = ivalidate(validation.mapConstraint(_.tupleLeft(constraint)).mapActual((actual, _)))(f)

//     final def ivalidate[C, D](validation: Validation[B, Constraint[C], C, D], writer: Writer[C])(
//         f: D => B
//     ): Self[A, D] = ivalidate(validation, writer, writer)(f)

//     final def transform[C, D, E](
//         transformation: Transformation[B, Constraint[(Writer[C], C)], (Writer[D], D), E]
//     ): Self[A, E] = ivalidate(transformation.validation)(transformation.apply)

//     final def transform[C, D, E](
//         transformation: Transformation[B, Constraint[C], D, E],
//         constraint: Writer[C],
//         actual: Writer[D]
//     ): Self[A, E] = transform(
//       transformation.mapValidation(_.mapConstraint(_.tupleLeft(constraint)).mapActual((actual, _)))
//     )

//     final def transform[C, D](transformation: Transformation[B, Constraint[C], C, D], writer: Writer[C]): Self[A, D] =
//       transform(transformation, writer, writer)

//     final def apply[C, D](transformation: Transformation[B, Constraint[C], C, D])(using
//         resolver: SchemaResolver[Writer, C]
//     ): Self[A, D] = transform(transformation, resolver.resolve)

// trait ValidationReaderOps[Self[_, _], Constraint[_]: Functor, Writer[_]]:
//   extension [A, B](self: Self[A, B])
//     def validate[C, D, E](validation: Validation[B, Constraint[(Writer[C], C)], (Writer[D], D), E]): Self[A, E]

//     final def validate[C, D, E](
//         validation: Validation[B, Constraint[C], D, E],
//         constraint: Writer[C],
//         actual: Writer[D]
//     ): Self[A, E] = validate(validation.mapConstraint(_.tupleLeft(constraint)).mapActual((actual, _)))

//     final def validate[C, D](validation: Validation[B, Constraint[C], C, D], writer: Writer[C]): Self[A, D] =
//       validate(validation, writer, writer)

//     final def validate_[C, D](validation: Validation[B, Constraint[(Writer[C], C)], (Writer[D], D), Unit]): Self[A, B] =
//       validate(validation.tap)

//     final def validate_[C, D](
//         validation: Validation[B, Constraint[C], D, Unit],
//         constraint: Writer[C],
//         actual: Writer[D]
//     ): Self[A, B] = validate_(validation.mapConstraint(_.tupleLeft(constraint)).mapActual((actual, _)))

//     final def validate_[C](validation: Validation[B, Constraint[C], C, Unit], writer: Writer[C]): Self[A, B] =
//       validate_(validation, writer, writer)

// trait SchemaOps[Self[_, _], Optional[_, _], Parent[_], Collection[_, _], Union[_, _]]:
//   extension [A, B](self: Self[A, B])
//     def collection: Collection[self.type, Vector[B]]
//     def imap[C](f: B => C)(g: C => B): Self[A, C]
//     def optional: Optional[self.type, Option[B]]
//     def orElse[C](schema: Parent[C]): Union[self.type | schema.type, Either[B, C]]

//   extension [A, B <: Matchable](self: Self[A, B])
//     final def |[C <: Matchable](schema: Parent[C]): Union[self.type | schema.type, B | C] =
//       self.orElse(schema)
//       ???

// trait SchemaIsomorphicOps[Self[_, _], Optional[_, _], Parent[_], Collection[_, _], Union[_, _]]
//     extends SchemaOps[Self, Optional, Parent, Collection, Union]
// given invariant[A]: Invariant[Self[A, *]]

// trait SchemaReaderOps[Self[_, _], Optional[_, _], Parent[_], Collection[_, _], Union[_, _]]
//     extends SchemaOps[Self, Optional, Parent, Collection, Union]:
//   given functor[A]: Functor[Self[A, *]]

// trait SchemaWriterOps[Self[_, _], Optional[_, _], Parent[_], Collection[_, _], Union[_, _]]
//     extends SchemaOps[Self, Optional, Parent, Collection, Union]:
//   given contravariant[A]: Contravariant[Self[A, *]]

// trait PrimitiveOps[Self[_], Optional[_], Parent[_], Collection[_, _], Union[_, _]]
//     extends SchemaOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Parent, Collection, Union]

// trait PrimitiveIsomorphicOps[Self[_], Optional[_], Parent[_], Collection[_, _], Union[_, _], Writer[_]]
//     extends SchemaIsomorphicOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Parent, Collection, Union],
//       PrimitiveOps[Self, Optional, Parent, Collection, Union],
//       ValidationIsomorphicOps[[_, a] =>> Self[a], Constraint.Primitive, Writer]

// trait PrimitiveReaderOps[Self[_], Optional[_], Parent[_], Collection[_, _], Union[_, _], Writer[_]]
//     extends SchemaReaderOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Parent, Collection, Union],
//       PrimitiveOps[Self, Optional, Parent, Collection, Union],
//       ValidationReaderOps[[_, a] =>> Self[a], Constraint.Primitive, Writer]

// trait PrimitiveWriterOps[Self[_], Optional[_], Parent[_], Collection[_, _], Union[_, _]]
//     extends SchemaWriterOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Parent, Collection, Union],
//       PrimitiveOps[Self, Optional, Parent, Collection, Union]

// trait CollectionOps[Self[_, _], Parent[_], Union[_, _]] extends SchemaOps[Self, Self, Parent, Self, Union]

// trait CollectionIsomporphicOps[Self[_, _], Parent[_], Union[_, _], Writer[_]]
//     extends SchemaIsomorphicOps[Self, Self, Parent, Self, Union],
//       CollectionOps[Self, Parent, Union],
//       ValidationIsomorphicOps[Self, [_] =>> Constraint.Collection, Writer]

// trait CollectionReaderOps[Self[_, _], Parent[_], Union[_, _], Writer[_]]
//     extends SchemaReaderOps[Self, Self, Parent, Self, Union],
//       CollectionOps[Self, Parent, Union],
//       ValidationReaderOps[Self, [_] =>> Constraint.Collection, Writer]

// trait CollectionWriterOps[Self[_, _], Parent[_], Union[_, _]]
//     extends SchemaWriterOps[Self, Self, Parent, Self, Union],
//       CollectionOps[Self, Parent, Union]
