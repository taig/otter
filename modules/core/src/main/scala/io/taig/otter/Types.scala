package io.taig.otter

import io.taig.otter as Base

trait Types:
  val container: Container

  export Base.{Constraint, SchemaValidation, Type, ValidationWriter}

  object ValidationInvariant:
    type Collection[F[_]] = Base.ValidationInvariant[[_] =>> Constraint.Collection, ValidationWriter, F]

    type Primitive[F[_]] = Base.ValidationInvariant[
      [a] =>> Constraint.Primitive[ValidationWriter.Value[a]],
      ValidationWriter.Value,
      F
    ]

  object ValidationFunctor:
    type Collection[F[_]] = Base.ValidationFunctor[[_] =>> Constraint.Collection, ValidationWriter, F]

    type Primitive[F[_]] = Base.ValidationFunctor[
      [a] =>> Constraint.Primitive[ValidationWriter.Value[a]],
      ValidationWriter.Value,
      F
    ]

  object ValidationContravariant:
    type Collection[F[_]] = Base.ValidationContravariant[
      [_] =>> Constraint.Collection,
      ValidationWriter,
      F
    ]

    type Primitive[F[_]] = Base.ValidationContravariant[
      [a] =>> Constraint.Primitive[ValidationWriter.Value[a]],
      ValidationWriter.Value,
      F
    ]

  final type Schema[A] = container.Schema[Base.Schema[container.Schema, ?, ?, A]]

  object Schema:
    type Of[A, B] = container.Schema[Base.Schema[container.Schema, ?, A, B]]
    type Via[A, B] = container.Schema[Base.Schema[container.Schema, A, ?, B]]
    type With[A, B, C] = container.Schema[Base.Schema[container.Schema, A, B, C]]
    type Any = container.Schema[Base.Schema[container.Schema, ?, ?, ?]]

    type Reader[A] = container.Schema[Base.Schema.Reader[container.Schema, ?, ?, A]]

    object Reader:
      type Of[A, B] = container.Schema[Base.Schema.Reader[container.Schema, ?, A, B]]
      type Via[A, B] = container.Schema[Base.Schema.Reader[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Schema[Base.Schema.Reader[container.Schema, A, B, C]]
      type Any = container.Schema[Base.Schema.Reader[container.Schema, ?, ?, ?]]

    type Writer[A] = container.Schema[Base.Schema.Writer[container.Schema, ?, ?, A]]

    object Writer:
      type Of[A, B] = container.Schema[Base.Schema.Writer[container.Schema, ?, A, B]]
      type Via[A, B] = container.Schema[Base.Schema.Writer[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Schema[Base.Schema.Writer[container.Schema, A, B, C]]
      type Any = container.Schema[Base.Schema.Writer[container.Schema, ?, ?, ?]]

  final type Value[A] = container.Schema[Base.Value[container.Schema, ?, ?, A]]

  object Value:
    type Of[A, B] = container.Schema[Base.Value[container.Schema, ?, A, B]]
    type Via[A, B] = container.Schema[Base.Value[container.Schema, A, ?, B]]
    type With[A, B, C] = container.Schema[Base.Value[container.Schema, A, B, C]]
    type Any = container.Schema[Base.Value[container.Schema, ?, ?, ?]]

    type Required[A] = container.Schema[Base.Value.Required[container.Schema, ?, ?, A]]

    object Required:
      type Of[A, B] = container.Schema[Base.Value.Required[container.Schema, ?, A, B]]
      type Via[A, B] = container.Schema[Base.Value.Required[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Schema[Base.Value.Required[container.Schema, A, B, C]]
      type Any = container.Schema[Base.Value.Required[container.Schema, ?, ?, ?]]

      type Reader[A] = container.Schema[Base.Value.Required.Reader[container.Schema, ?, ?, A]]

      object Reader:
        type Of[A, B] = container.Schema[Base.Value.Required.Reader[container.Schema, ?, A, B]]
        type Via[A, B] = container.Schema[Base.Value.Required.Reader[container.Schema, A, ?, B]]
        type With[A, B, C] = container.Schema[Base.Value.Required.Reader[container.Schema, A, B, C]]
        type Any = container.Schema[Base.Value.Required.Reader[container.Schema, ?, ?, ?]]

      type Writer[A] = container.Schema[Base.Value.Required.Writer[container.Schema, ?, ?, A]]

      object Writer:
        type Of[A, B] = container.Schema[Base.Value.Required.Writer[container.Schema, ?, A, B]]
        type Via[A, B] = container.Schema[Base.Value.Required.Writer[container.Schema, A, ?, B]]
        type With[A, B, C] = container.Schema[Base.Value.Required.Writer[container.Schema, A, B, C]]
        type Any = container.Schema[Base.Value.Required.Writer[container.Schema, ?, ?, ?]]

    type Reader[A] = container.Schema[Base.Value.Reader[container.Schema, ?, ?, A]]

    object Reader:
      type Of[A, B] = container.Schema[Base.Value.Reader[container.Schema, ?, A, B]]
      type Via[A, B] = container.Schema[Base.Value.Reader[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Schema[Base.Value.Reader[container.Schema, A, B, C]]
      type Any = container.Schema[Base.Value.Reader[container.Schema, ?, ?, ?]]

    type Writer[A] = container.Schema[Base.Value.Writer[container.Schema, ?, ?, A]]

    object Writer:
      type Of[A, B] = container.Schema[Base.Value.Writer[container.Schema, ?, A, B]]
      type Via[A, B] = container.Schema[Base.Value.Writer[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Schema[Base.Value.Writer[container.Schema, A, B, C]]
      type Any = container.Schema[Base.Value.Writer[container.Schema, ?, ?, ?]]

  final type Collection[A] = container.Collection[Base.Collection[container.Schema, ?, ?, A]]

  object Collection:
    type Of[A, B] = container.Collection[Base.Collection[container.Schema, ?, A, B]]
    type Via[A, B] = container.Collection[Base.Collection[container.Schema, A, ?, B]]
    type With[A, B, C] = container.Collection[Base.Collection[container.Schema, A, B, C]]
    type Any = container.Collection[Base.Collection[container.Schema, ?, ?, ?]]

    type Reader[A] = container.Collection[Base.Collection.Reader[container.Schema, ?, ?, A]]

    object Reader:
      type Of[A, B] = container.Collection[Base.Collection.Reader[container.Schema, ?, A, B]]
      type Via[A, B] = container.Collection[Base.Collection.Reader[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Collection[Base.Collection.Reader[container.Schema, A, B, C]]
      type Any = container.Collection[Base.Collection.Reader[container.Schema, ?, ?, ?]]

    type Writer[A] = container.Collection[Base.Collection.Writer[container.Schema, ?, ?, A]]

    object Writer:
      type Of[A, B] = container.Collection[Base.Collection.Writer[container.Schema, ?, A, B]]
      type Via[A, B] = container.Collection[Base.Collection.Writer[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Collection[Base.Collection.Writer[container.Schema, A, B, C]]
      type Any = container.Collection[Base.Collection.Writer[container.Schema, ?, ?, ?]]

  final type Dictionary[A] = container.Dictionary[Base.Dictionary[container.Schema, ?, ?, A]]

  object Dictionary:
    type Of[A, B] = container.Dictionary[Base.Dictionary[container.Schema, ?, A, B]]
    type Via[A, B] = container.Dictionary[Base.Dictionary[container.Schema, A, ?, B]]
    type With[A, B, C] = container.Dictionary[Base.Dictionary[container.Schema, A, B, C]]
    type Any = container.Dictionary[Base.Dictionary[container.Schema, ?, ?, ?]]

    type Reader[A] = container.Dictionary[Base.Dictionary.Reader[container.Schema, ?, ?, A]]

    object Reader:
      type Of[A, B] = container.Dictionary[Base.Dictionary.Reader[container.Schema, ?, A, B]]
      type Via[A, B] = container.Dictionary[Base.Dictionary.Reader[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Dictionary[Base.Dictionary.Reader[container.Schema, A, B, C]]
      type Any = container.Dictionary[Base.Dictionary.Reader[container.Schema, ?, ?, ?]]

    type Writer[A] = container.Dictionary[Base.Dictionary.Writer[container.Schema, ?, ?, A]]

    object Writer:
      type Of[A, B] = container.Dictionary[Base.Dictionary.Writer[container.Schema, ?, A, B]]
      type Via[A, B] = container.Dictionary[Base.Dictionary.Writer[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Dictionary[Base.Dictionary.Writer[container.Schema, A, B, C]]
      type Any = container.Dictionary[Base.Dictionary.Writer[container.Schema, ?, ?, ?]]

  final type Dynamic[A, B] = container.Dynamic[Base.Dynamic[A, B]]

  object Dynamic:
    type Any[A] = container.Dynamic[Base.Dynamic[A, ?]]

    type Reader[A, B] = container.Dynamic[Base.Dynamic.Reader[A, B]]

    object Reader:
      type Any[A] = container.Dynamic[Base.Dynamic.Reader[A, ?]]

    type Writer[A, B] = container.Dynamic[Base.Dynamic.Writer[A, B]]

    object Writer:
      type Any[A] = container.Dynamic[Base.Dynamic.Writer[A, ?]]

  final type Enumeration[A] = container.Enumeration[Base.Enumeration[container.Schema, ?, ?, A]]

  object Enumeration:
    type Of[A, B] = container.Enumeration[Base.Enumeration.Required[container.Schema, ?, A, B]]
    type Via[A, B] = container.Enumeration[Base.Enumeration.Required[container.Schema, A, ?, B]]
    type With[A, B, C] = container.Enumeration[Base.Enumeration.Required[container.Schema, A, B, C]]
    type Any = container.Enumeration[Base.Enumeration.Required[container.Schema, ?, ?, ?]]

    type Required[A] = container.Enumeration[Base.Enumeration.Required[container.Schema, ?, ?, A]]

    object Required:
      type Of[A, B] = container.Enumeration[Base.Enumeration.Required[container.Schema, ?, A, B]]
      type Via[A, B] = container.Enumeration[Base.Enumeration.Required[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Enumeration[Base.Enumeration.Required[container.Schema, A, B, C]]
      type Any = container.Enumeration[Base.Enumeration.Required[container.Schema, ?, ?, ?]]

      type Reader[A] = container.Enumeration[Base.Enumeration.Required.Reader[container.Schema, ?, ?, A]]

      object Reader:
        type Of[A, B] = container.Enumeration[Base.Enumeration.Required.Reader[container.Schema, ?, A, B]]
        type Via[A, B] = container.Enumeration[Base.Enumeration.Required.Reader[container.Schema, A, ?, B]]
        type With[A, B, C] = container.Enumeration[Base.Enumeration.Required.Reader[container.Schema, A, B, C]]
        type Any = container.Enumeration[Base.Enumeration.Required.Reader[container.Schema, ?, ?, ?]]

      type Writer[A] = container.Enumeration[Base.Enumeration.Required.Writer[container.Schema, ?, ?, A]]

      object Writer:
        type Of[A, B] = container.Enumeration[Base.Enumeration.Required.Writer[container.Schema, ?, A, B]]
        type Via[A, B] = container.Enumeration[Base.Enumeration.Required.Writer[container.Schema, A, ?, B]]
        type With[A, B, C] = container.Enumeration[Base.Enumeration.Required.Writer[container.Schema, A, B, C]]
        type Any = container.Enumeration[Base.Enumeration.Required.Writer[container.Schema, ?, ?, ?]]

    type Reader[A] = container.Enumeration[Base.Enumeration.Reader[container.Schema, ?, ?, A]]

    object Reader:
      type Of[A, B] = container.Enumeration[Base.Enumeration.Reader[container.Schema, ?, A, B]]
      type Via[A, B] = container.Enumeration[Base.Enumeration.Reader[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Enumeration[Base.Enumeration.Reader[container.Schema, A, B, C]]
      type Any = container.Enumeration[Base.Enumeration.Reader[container.Schema, ?, ?, ?]]

    type Writer[A] = container.Enumeration[Base.Enumeration.Writer[container.Schema, ?, ?, A]]

    object Writer:
      type Of[A, B] = container.Enumeration[Base.Enumeration.Writer[container.Schema, ?, A, B]]
      type Via[A, B] = container.Enumeration[Base.Enumeration.Writer[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Enumeration[Base.Enumeration.Writer[container.Schema, A, B, C]]
      type Any = container.Enumeration[Base.Enumeration.Writer[container.Schema, ?, ?, ?]]

  final type Primitive[A] = container.Primitive[Base.Primitive[A]]

  object Primitive:
    type Any = container.Primitive[Base.Primitive.Required[?]]

    type Required[A] = container.Primitive[Base.Primitive.Required[A]]

    object Required:
      type Any = container.Primitive[Base.Primitive.Required[?]]

      type Reader[A] = container.Primitive[Base.Primitive.Required.Reader[A]]

      object Reader:
        type Any = container.Primitive[Base.Primitive.Required.Reader[?]]

      type Writer[A] = container.Primitive[Base.Primitive.Required.Writer[A]]

      object Writer:
        type Any = container.Primitive[Base.Primitive.Required.Writer[?]]

    type Reader[A] = container.Primitive[Base.Primitive.Reader[A]]

    object Reader:
      type Any = container.Primitive[Base.Primitive.Reader[?]]

    type Writer[A] = container.Primitive[Base.Primitive.Writer[A]]

    object Writer:
      type Any = container.Primitive[Base.Primitive.Writer[?]]

  final type Product[A] = container.Product[Base.Product[container.Schema, ?, ?, A]]

  object Product:
    type Of[A, B] = container.Product[Base.Product[container.Schema, ?, A, B]]
    type Via[A, B] = container.Product[Base.Product[container.Schema, A, ?, B]]
    type With[A, B, C] = container.Product[Base.Product[container.Schema, A, B, C]]
    type Any = container.Product[Base.Product[container.Schema, ?, ?, ?]]

    type Reader[A] = container.Product[Base.Product.Reader[container.Schema, ?, ?, A]]

    object Reader:
      type Of[A, B] = container.Product[Base.Product.Reader[container.Schema, ?, A, B]]
      type Via[A, B] = container.Product[Base.Product.Reader[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Product[Base.Product.Reader[container.Schema, A, B, C]]
      type Any = container.Product[Base.Product.Reader[container.Schema, ?, ?, ?]]

    type Writer[A] = container.Product[Base.Product.Writer[container.Schema, ?, ?, A]]

    object Writer:
      type Of[A, B] = container.Product[Base.Product.Writer[container.Schema, ?, A, B]]
      type Via[A, B] = container.Product[Base.Product.Writer[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Product[Base.Product.Writer[container.Schema, A, B, C]]
      type Any = container.Product[Base.Product.Writer[container.Schema, ?, ?, ?]]

  final type Record[A] = container.Record[Base.Record[container.Schema, ?, ?, A]]

  object Record:
    type Of[A, B] = container.Record[Base.Record[container.Schema, ?, A, B]]
    type Via[A, B] = container.Record[Base.Record[container.Schema, A, ?, B]]
    type With[A, B, C] = container.Record[Base.Record[container.Schema, A, B, C]]
    type Any = container.Record[Base.Record[container.Schema, ?, ?, ?]]

    type Reader[A] = container.Record[Base.Record.Reader[container.Schema, ?, ?, A]]

    object Reader:
      type Of[A, B] = container.Record[Base.Record.Reader[container.Schema, ?, A, B]]
      type Via[A, B] = container.Record[Base.Record.Reader[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Record[Base.Record.Reader[container.Schema, A, B, C]]
      type Any = container.Record[Base.Record.Reader[container.Schema, ?, ?, ?]]

    type Writer[A] = container.Record[Base.Record.Writer[container.Schema, ?, ?, A]]

    object Writer:
      type Of[A, B] = container.Record[Base.Record.Writer[container.Schema, ?, A, B]]
      type Via[A, B] = container.Record[Base.Record.Writer[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Record[Base.Record.Writer[container.Schema, A, B, C]]
      type Any = container.Record[Base.Record.Writer[container.Schema, ?, ?, ?]]

    export Base.Record.Null

  final type Sum[A] = container.Sum[Base.Sum[container.Schema, ?, ?, A]]

  object Sum:
    export Base.Sum.Discriminator

    type Of[A, B] = container.Sum[Base.Sum[container.Schema, ?, A, B]]
    type Via[A, B] = container.Sum[Base.Sum[container.Schema, A, ?, B]]
    type With[A, B, C] = container.Sum[Base.Sum[container.Schema, A, B, C]]
    type Any = container.Sum[Base.Sum[container.Schema, ?, ?, ?]]

    type Reader[A] = container.Sum[Base.Sum.Reader[container.Schema, ?, ?, A]]

    object Reader:
      type Of[A, B] = container.Sum[Base.Sum.Reader[container.Schema, ?, A, B]]
      type Via[A, B] = container.Sum[Base.Sum.Reader[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Sum[Base.Sum.Reader[container.Schema, A, B, C]]
      type Any = container.Sum[Base.Sum.Reader[container.Schema, ?, ?, ?]]

    type Writer[A] = container.Sum[Base.Sum.Writer[container.Schema, ?, ?, A]]

    object Writer:
      type Of[A, B] = container.Sum[Base.Sum.Writer[container.Schema, ?, A, B]]
      type Via[A, B] = container.Sum[Base.Sum.Writer[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Sum[Base.Sum.Writer[container.Schema, A, B, C]]
      type Any = container.Sum[Base.Sum.Writer[container.Schema, ?, ?, ?]]

  final type Union[A] = container.Union[Base.Union[container.Schema, ?, ?, A]]

  object Union:
    type Of[A, B] = container.Union[Base.Union[container.Schema, ?, A, B]]
    type Via[A, B] = container.Union[Base.Union[container.Schema, A, ?, B]]
    type With[A, B, C] = container.Union[Base.Union[container.Schema, A, B, C]]
    type Any = container.Union[Base.Union[container.Schema, ?, ?, ?]]

    type Value[A] = container.Union[Base.Union.Value[container.Schema, ?, ?, A]]

    object Value:
      type Of[A, B] = container.Union[Base.Union.Value[container.Schema, ?, A, B]]
      type Via[A, B] = container.Union[Base.Union.Value[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Union[Base.Union.Value[container.Schema, A, B, C]]
      type Any = container.Union[Base.Union.Value[container.Schema, ?, ?, ?]]

      type Required[A] = container.Union[Base.Union.Value.Required[container.Schema, ?, ?, A]]

      object Required:
        type Of[A, B] = container.Union[Base.Union.Value.Required[container.Schema, ?, A, B]]
        type Via[A, B] = container.Union[Base.Union.Value.Required[container.Schema, A, ?, B]]
        type With[A, B, C] = container.Union[Base.Union.Value.Required[container.Schema, A, B, C]]
        type Any = container.Union[Base.Union.Value.Required[container.Schema, ?, ?, ?]]

        type Reader[A] = container.Union[Base.Union.Value.Required.Reader[container.Schema, ?, ?, A]]

        object Reader:
          type Of[A, B] = container.Union[Base.Union.Value.Required.Reader[container.Schema, ?, A, B]]
          type Via[A, B] = container.Union[Base.Union.Value.Required.Reader[container.Schema, A, ?, B]]
          type With[A, B, C] = container.Union[Base.Union.Value.Required.Reader[container.Schema, A, B, C]]
          type Any = container.Union[Base.Union.Value.Required.Reader[container.Schema, ?, ?, ?]]

        type Writer[A] = container.Union[Base.Union.Value.Required.Writer[container.Schema, ?, ?, A]]

        object Writer:
          type Of[A, B] = container.Union[Base.Union.Value.Required.Writer[container.Schema, ?, A, B]]
          type Via[A, B] = container.Union[Base.Union.Value.Required.Writer[container.Schema, A, ?, B]]
          type With[A, B, C] = container.Union[Base.Union.Value.Required.Writer[container.Schema, A, B, C]]
          type Any = container.Union[Base.Union.Value.Required.Writer[container.Schema, ?, ?, ?]]

      type Reader[A] = container.Union[Base.Union.Value.Reader[container.Schema, ?, ?, A]]

      object Reader:
        type Of[A, B] = container.Union[Base.Union.Value.Reader[container.Schema, ?, A, B]]
        type Via[A, B] = container.Union[Base.Union.Value.Reader[container.Schema, A, ?, B]]
        type With[A, B, C] = container.Union[Base.Union.Value.Reader[container.Schema, A, B, C]]
        type Any = container.Union[Base.Union.Value.Reader[container.Schema, ?, ?, ?]]

      type Writer[A] = container.Union[Base.Union.Value.Writer[container.Schema, ?, ?, A]]

      object Writer:
        type Of[A, B] = container.Union[Base.Union.Value.Writer[container.Schema, ?, A, B]]
        type Via[A, B] = container.Union[Base.Union.Value.Writer[container.Schema, A, ?, B]]
        type With[A, B, C] = container.Union[Base.Union.Value.Writer[container.Schema, A, B, C]]
        type Any = container.Union[Base.Union.Value.Writer[container.Schema, ?, ?, ?]]

    type Reader[A] = container.Union[Base.Union.Reader[container.Schema, ?, ?, A]]

    object Reader:
      type Of[A, B] = container.Union[Base.Union.Reader[container.Schema, ?, A, B]]
      type Via[A, B] = container.Union[Base.Union.Reader[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Union[Base.Union.Reader[container.Schema, A, B, C]]
      type Any = container.Union[Base.Union.Reader[container.Schema, ?, ?, ?]]

    type Writer[A] = container.Union[Base.Union.Writer[container.Schema, ?, ?, A]]

    object Writer:
      type Of[A, B] = container.Union[Base.Union.Writer[container.Schema, ?, A, B]]
      type Via[A, B] = container.Union[Base.Union.Writer[container.Schema, A, ?, B]]
      type With[A, B, C] = container.Union[Base.Union.Writer[container.Schema, A, B, C]]
      type Any = container.Union[Base.Union.Writer[container.Schema, ?, ?, ?]]

  final type Branch[A] = Base.Branch[container.Schema, ?, ?, A]

  object Branch:
    type Of[A, B] = Base.Branch[container.Schema, ?, A, B]
    type Via[A, B] = Base.Branch[container.Schema, A, ?, B]
    type With[A, B, C] = Base.Branch[container.Schema, A, B, C]
    type Any = Base.Branch[container.Schema, ?, ?, ?]

    type Reader[A] = Base.Branch.Reader[container.Schema, ?, ?, A]

    object Reader:
      type Of[A, B] = Base.Branch.Reader[container.Schema, ?, A, B]
      type Via[A, B] = Base.Branch.Reader[container.Schema, A, ?, B]
      type With[A, B, C] = Base.Branch.Reader[container.Schema, A, B, C]
      type Any = Base.Branch.Reader[container.Schema, ?, ?, ?]

    type Writer[A] = Base.Branch.Writer[container.Schema, ?, ?, A]

    object Writer:
      type Of[A, B] = Base.Branch.Writer[container.Schema, ?, A, B]
      type Via[A, B] = Base.Branch.Writer[container.Schema, A, ?, B]
      type With[A, B, C] = Base.Branch.Writer[container.Schema, A, B, C]
      type Any = Base.Branch.Writer[container.Schema, ?, ?, ?]

  final type Field[A] = Base.Field[container.Schema, ?, ?, A]

  object Field:
    export Base.Field.Null

    type Of[A, B] = Base.Field[container.Schema, ?, A, B]
    type Via[A, B] = Base.Field[container.Schema, A, ?, B]
    type With[A, B, C] = Base.Field[container.Schema, A, B, C]
    type Any = Base.Field[container.Schema, ?, ?, ?]

    type Reader[A] = Base.Field.Reader[container.Schema, ?, ?, A]

    object Reader:
      type Of[A, B] = Base.Field.Reader[container.Schema, ?, A, B]
      type Via[A, B] = Base.Field.Reader[container.Schema, A, ?, B]
      type With[A, B, C] = Base.Field.Reader[container.Schema, A, B, C]
      type Any = Base.Field.Reader[container.Schema, ?, ?, ?]

    type Writer[A] = Base.Field.Writer[container.Schema, ?, ?, A]

    object Writer:
      type Of[A, B] = Base.Field.Writer[container.Schema, ?, A, B]
      type Via[A, B] = Base.Field.Writer[container.Schema, A, ?, B]
      type With[A, B, C] = Base.Field.Writer[container.Schema, A, B, C]
      type Any = Base.Field.Writer[container.Schema, ?, ?, ?]
