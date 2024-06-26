package io.taig.otter

import cats.Functor
import cats.Invariant
import cats.Contravariant
import cats.syntax.all.*
import io.taig.otter.validation.Validation

trait SchemaOps[Self[_, _], Optional[_, _], Collection[_, _]]:
  extension [A, B](self: Self[A, B])
    def collection: Collection[self.type, Vector[B]]
    def optional: Optional[self.type, Option[B]]

trait ValidationIsomorphicOps[Self[_, _], Constraint[_]: Functor, Writer[_]]:
  extension [A, B](self: Self[A, B])
    def ivalidate[C, D, E](validation: Validation[B, Constraint[(Writer[C], C)], (Writer[D], D), E])(
        f: E => B
    ): Self[A, E]

    final def ivalidate[C, D, E](
        validation: Validation[B, Constraint[C], D, E],
        constraint: Writer[C],
        actual: Writer[D]
    )(
        f: E => B
    ): Self[A, E] = ivalidate(validation.mapConstraint(_.tupleLeft(constraint)).mapActual((actual, _)))(f)

    final def ivalidate[C, D](validation: Validation[B, Constraint[C], C, D], writer: Writer[C])(
        f: D => B
    ): Self[A, D] = ivalidate(validation, writer, writer)(f)

    final def transform[C, D, E](
        transformation: Transformation[B, Constraint[(Writer[C], C)], (Writer[D], D), E]
    ): Self[A, E] = ivalidate(transformation.validation)(transformation.apply)

    final def transform[C, D, E](
        transformation: Transformation[B, Constraint[C], D, E],
        constraint: Writer[C],
        actual: Writer[D]
    ): Self[A, E] = transform(
      transformation.mapValidation(_.mapConstraint(_.tupleLeft(constraint)).mapActual((actual, _)))
    )

    final def transform[C, D](transformation: Transformation[B, Constraint[C], C, D], writer: Writer[C]): Self[A, D] =
      transform(transformation, writer, writer)

    final def apply[C, D](transformation: Transformation[B, Constraint[C], C, D])(using
        resolver: SchemaResolver[Writer, C]
    ): Self[A, D] = transform(transformation, resolver.resolve)

trait ValidationReaderOps[Self[_, _], Constraint[_]: Functor, Writer[_]]:
  extension [A, B](self: Self[A, B])
    def validate[C, D, E](validation: Validation[B, Constraint[(Writer[C], C)], (Writer[D], D), E]): Self[A, E]

    final def validate[C, D, E](
        validation: Validation[B, Constraint[C], D, E],
        constraint: Writer[C],
        actual: Writer[D]
    ): Self[A, E] = validate(validation.mapConstraint(_.tupleLeft(constraint)).mapActual((actual, _)))

    final def validate[C, D](validation: Validation[B, Constraint[C], C, D], writer: Writer[C]): Self[A, D] =
      validate(validation, writer, writer)

    final def validate_[C, D](validation: Validation[B, Constraint[(Writer[C], C)], (Writer[D], D), Unit]): Self[A, B] =
      validate(validation.tap)

    final def validate_[C, D](
        validation: Validation[B, Constraint[C], D, Unit],
        constraint: Writer[C],
        actual: Writer[D]
    ): Self[A, B] = validate_(validation.mapConstraint(_.tupleLeft(constraint)).mapActual((actual, _)))

    final def validate_[C](validation: Validation[B, Constraint[C], C, Unit], writer: Writer[C]): Self[A, B] =
      validate_(validation, writer, writer)

trait SchemaIsomorphicOps[Self[_, _], Optional[_, _], Collection[_, _]] extends SchemaOps[Self, Optional, Collection]:
  given invariant[A]: Invariant[Self[A, *]]

trait SchemaReaderOps[Self[_, _], Optional[_, _], Collection[_, _]] extends SchemaOps[Self, Optional, Collection]:
  given functor[A]: Functor[Self[A, *]]

trait SchemaWriterOps[Self[_, _], Optional[_, _], Collection[_, _]] extends SchemaOps[Self, Optional, Collection]:
  given contravariant[A]: Contravariant[Self[A, *]]

trait PrimitiveOps[Self[_], Optional[_], Collection[_, _]]
    extends SchemaOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection]

trait PrimitiveIsomorphicOps[Self[_], Optional[_], Collection[_, _], Writer[_]]
    extends SchemaIsomorphicOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection],
      PrimitiveOps[Self, Optional, Collection],
      ValidationIsomorphicOps[[_, a] =>> Self[a], Constraint.Primitive, Writer]

trait PrimitiveReaderOps[Self[_], Optional[_], Collection[_, _], Writer[_]]
    extends SchemaReaderOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection],
      PrimitiveOps[Self, Optional, Collection],
      ValidationReaderOps[[_, a] =>> Self[a], Constraint.Primitive, Writer]

trait PrimitiveWriterOps[Self[_], Optional[_], Collection[_, _]]
    extends SchemaWriterOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection],
      PrimitiveOps[Self, Optional, Collection]

trait CollectionOps[Self[_, _]] extends SchemaOps[Self, Self, Self]

trait CollectionIsomporphicOps[Self[_, _], Writer[_]]
    extends SchemaIsomorphicOps[Self, Self, Self],
      CollectionOps[Self],
      ValidationIsomorphicOps[Self, [_] =>> Constraint.Collection, Writer]

trait CollectionReaderOps[Self[_, _], Writer[_]]
    extends SchemaReaderOps[Self, Self, Self],
      CollectionOps[Self],
      ValidationReaderOps[Self, [_] =>> Constraint.Collection, Writer]

trait CollectionWriterOps[Self[_, _]] extends SchemaWriterOps[Self, Self, Self], CollectionOps[Self]
