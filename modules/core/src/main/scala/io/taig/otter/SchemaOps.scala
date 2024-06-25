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
  def invariant[A]: Invariant[Self[A, *]]

trait SchemaReaderOps[Self[_, _], Optional[_, _], Collection[_, _]] extends SchemaOps[Self, Optional, Collection]:
  def functor[A]: Functor[Self[A, *]]

trait SchemaWriterOps[Self[_, _], Optional[_, _], Collection[_, _]] extends SchemaOps[Self, Optional, Collection]:
  def contravariant[A]: Contravariant[Self[A, *]]

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
      ValidationIsomorphicOps[Self, [_] =>> Constraint.Collection, Writer]:
  extension [A, B](self: Self[A, Vector[B]])
    final def apply[C, D](builder: CollectionBuilder[Vector[B], C, D], writer: Writer[C]): Self[A, D] =
      self.ivalidate(builder.validation, writer)(builder.from)

trait CollectionReaderOps[Self[_, _], Writer[_]]
    extends SchemaReaderOps[Self, Self, Self],
      CollectionOps[Self],
      ValidationReaderOps[Self, [_] =>> Constraint.Collection, Writer]:
  extension [A, B](self: Self[A, Vector[B]])
    final def apply[C, D](builder: CollectionBuilder.Reader[Vector[B], C, D], writer: Writer[C]): Self[A, D] =
      self.validate(builder.validation, writer)

trait CollectionWriterOps[Self[_, _]] extends SchemaWriterOps[Self, Self, Self], CollectionOps[Self]:
  extension [A, B](self: Self[A, Vector[B]])
    final def apply[C](builder: CollectionBuilder.Writer[Vector[B], C]): Self[A, C] =
      contravariant[A].contramap(self)(builder.from)
