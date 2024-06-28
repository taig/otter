package io.taig.otter

import io.taig.otter as Base

trait Types:
  val container: Container

  final type Constraint = Base.Constraint
  val Constraint: Base.Constraint.type = Base.Constraint

  final type SchemaValidation[Constraint[a] <: Constraint.Any[a], A, B, C, D] =
    Base.SchemaValidation[Schema.Writer, Constraint, A, B, C, D]

  object SchemaValidation:
    type Primitive[A, B, C, D] = Base.SchemaValidation.Primitive[Schema.Writer, A, B, C, D]

  final type ValidationInvariant[Constraint[a] <: Constraint.Any[a], Self[_]] =
    Base.ValidationInvariant[Schema.Writer, Constraint, Self]

  object ValidationInvariant:
    type Primitive[Self[_]] = ValidationInvariant[Constraint.Primitive, Self]

  final type ValidationFunctor[Constraint[a] <: Constraint.Any[a], Self[_]] =
    Base.ValidationFunctor[Schema.Writer, Constraint, Self]

  object ValidationFunctor:
    type Primitive[Self[_]] = ValidationFunctor[Constraint.Primitive, Self]

  final type ValidationContravariant[Constraint[a] <: Constraint.Any[a], Self[_]] =
    Base.ValidationContravariant[Schema.Writer, Constraint, Self]

  object ValidationContravariant:
    type Primitive[Self[_]] = ValidationContravariant[Constraint.Primitive, Self]

  object PrimitiveOps:
    type Isomorphic[Self[_], Optional[_]] = Base.PrimitiveOps.Isomorphic[Self, Optional, Collection.Of, Union.Of]
    type Reader[Self[_], Optional[_]] = Base.PrimitiveOps.Reader[Self, Optional, Collection.Reader.Of, Union.Reader.Of]
    type Writer[Self[_], Optional[_]] = Base.PrimitiveOps.Writer[Self, Optional, Collection.Writer.Of, Union.Writer.Of]

  final type Schema[A] = container.Schema[Base.Schema[container.Schema, ?, A]]

  object Schema:
    type Of[A, B] = container.Schema[Base.Schema[container.Schema, A, B]]
    type Any = container.Schema[Base.Schema[container.Schema, ?, ?]]

    type Reader[A] = container.Schema[Base.Schema.Reader[container.Schema, ?, A]]

    object Reader:
      type Of[A, B] = container.Schema[Base.Schema.Reader[container.Schema, A, B]]
      type Any = container.Schema[Base.Schema.Reader[container.Schema, ?, ?]]

    type Writer[A] = container.Schema[Base.Schema.Writer[container.Schema, ?, A]]

    object Writer:
      type Of[A, B] = container.Schema[Base.Schema.Writer[container.Schema, A, B]]
      type Any = container.Schema[Base.Schema.Writer[container.Schema, ?, ?]]

  final type Collection[A] = container.Collection[Base.Collection[container.Schema, ?, A]]

  object Collection:
    type Of[A, B] = container.Collection[Base.Collection[container.Schema, A, B]]
    type Any = container.Collection[Base.Collection[container.Schema, ?, ?]]

    type Reader[A] = container.Collection[Base.Collection.Reader[container.Schema, ?, A]]

    object Reader:
      type Of[A, B] = container.Collection[Base.Collection.Reader[container.Schema, A, B]]
      type Any = container.Collection[Base.Collection.Reader[container.Schema, ?, ?]]

    type Writer[A] = container.Collection[Base.Collection.Writer[container.Schema, ?, A]]

    object Writer:
      type Of[A, B] = container.Collection[Base.Collection.Writer[container.Schema, A, B]]
      type Any = container.Collection[Base.Collection.Writer[container.Schema, ?, ?]]

  final type Primitive[A] = container.Primitive[Base.Primitive[container.Schema, A]]

  object Primitive:
    type Any = container.Primitive[Base.Primitive.Required[container.Schema, ?]]

    type Required[A] = container.Primitive[Base.Primitive.Required[container.Schema, A]]

    object Required:
      type Any = container.Primitive[Base.Primitive.Required[container.Schema, ?]]

      type Reader[A] = container.Primitive[Base.Primitive.Required.Reader[container.Schema, A]]

      object Reader:
        type Any = container.Primitive[Base.Primitive.Required.Reader[container.Schema, ?]]

      type Writer[A] = container.Primitive[Base.Primitive.Required.Writer[container.Schema, A]]

      object Writer:
        type Any = container.Primitive[Base.Primitive.Required.Writer[container.Schema, ?]]

    type Reader[A] = container.Primitive[Base.Primitive.Reader[container.Schema, A]]

    object Reader:
      type Any = container.Primitive[Base.Primitive.Reader[container.Schema, ?]]

    type Writer[A] = container.Primitive[Base.Primitive.Writer[container.Schema, A]]

    object Writer:
      type Any = container.Primitive[Base.Primitive.Writer[container.Schema, ?]]

  final type Union[A] = container.Union[Base.Union[container.Schema, ?, A]]

  object Union:
    type Of[A, B] = container.Union[Base.Union[container.Schema, A, B]]
    type Any = container.Union[Base.Union[container.Schema, ?, ?]]

    type Reader[A] = container.Union[Base.Union.Reader[container.Schema, ?, A]]

    object Reader:
      type Of[A, B] = container.Union[Base.Union.Reader[container.Schema, A, B]]
      type Any = container.Union[Base.Union.Reader[container.Schema, ?, ?]]

    type Writer[A] = container.Union[Base.Union.Writer[container.Schema, ?, A]]

    object Writer:
      type Of[A, B] = container.Union[Base.Union.Writer[container.Schema, A, B]]
      type Any = container.Union[Base.Union.Writer[container.Schema, ?, ?]]
