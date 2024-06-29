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

  object ValidationInvariant:
    type Collection[F[_]] = Base.ValidationInvariant[[_] =>> Constraint.Collection, [a] =>> (Schema.Writer[a], a), F]
    type Primitive[F[_]] = Base.ValidationInvariant[
      [a] =>> Constraint.Primitive[(Schema.Writer[a], a)],
      [a] =>> (Schema.Writer[a], a),
      F
      ]
      
  object ValidationFunctor:
    type Collection[F[_]] = Base.ValidationFunctor[[_] =>> Constraint.Collection, [a] =>> (Schema.Writer[a], a), F]
    type Primitive[F[_]] = Base.ValidationFunctor[
      [a] =>> Constraint.Primitive[(Schema.Writer[a], a)],
      [a] =>> (Schema.Writer[a], a),
      F
      ]

  object ValidationContravariant:
    type Collection[F[_]] = Base.ValidationContravariant[[_] =>> Constraint.Collection, [a] =>> (Schema.Writer[a], a), F]
    type Primitive[F[_]] = Base.ValidationContravariant[
      [a] =>> Constraint.Primitive[(Schema.Writer[a], a)],
      [a] =>> (Schema.Writer[a], a),
      F
      ]

  object SchemaOps:
    type Isomorphic = Base.SchemaOps.Isomorphic[Schema.Of, Schema.Of, Schema.Of, Collection.Of, Union.Of]
    type Reader =
      Base.SchemaOps.Reader[Schema.Reader.Of, Schema.Writer.Of, Schema.Writer.Of, Collection.Reader.Of, Union.Reader.Of]
    type Writer =
      Base.SchemaOps.Writer[Schema.Writer.Of, Schema.Writer.Of, Schema.Writer.Of, Collection.Writer.Of, Union.Writer.Of]

  object CollectionOps:
    type Isomorphic = Base.CollectionOps.Isomorphic[Collection.Of, Union.Of, Schema.Of, Schema.Writer, Schema.Any]
    type Reader = Base.CollectionOps.Reader[
      Collection.Reader.Of,
      Union.Reader.Of,
      Schema.Reader.Of,
      Schema.Writer,
      Schema.Reader.Any
    ]
    type Writer = Base.CollectionOps.Writer[
      Collection.Writer.Of,
      Union.Writer.Of,
      Schema.Writer.Of,
      Schema.Writer,
      Schema.Writer.Any
    ]

  object PrimitiveOps:
    type Isomorphic[Self[_], Optional[_]] =
      Base.PrimitiveOps.Isomorphic[Self, Optional, Schema.Of, Collection.Of, Union.Of]
    type Reader[Self[_], Optional[_]] =
      Base.PrimitiveOps.Reader[Self, Optional, Schema.Reader.Of, Collection.Reader.Of, Union.Reader.Of]
    type Writer[Self[_], Optional[_]] =
      Base.PrimitiveOps.Writer[Self, Optional, Schema.Writer.Of, Collection.Writer.Of, Union.Writer.Of]

  object UnionOps:
    type Isomorphic = Base.UnionOps.Isomorphic[Union.Of, Schema.Of, Collection.Of]
    type Reader = Base.UnionOps.Reader[Union.Reader.Of, Schema.Reader.Of, Collection.Reader.Of]
    type Writer = Base.UnionOps.Writer[Union.Writer.Of, Schema.Writer.Of, Collection.Writer.Of]

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
