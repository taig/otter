package io.taig.otter

import io.taig.otter as Base

trait Types:
  export Base.{Comparison, Constraint, Data, Metadata, Step, Violation, Violations}

  final type Codec[A] = Base.Codec[Data.Optional, Data, A]

  object Codec:
    type Of[O <: Data, A] = Base.Codec[Data.Optional, O, A]

    type Required[A] = Base.Codec[Data.Required, Data, A]

    object Required:
      type Of[O <: Data, A] = Base.Codec[Data.Required, O, A]

    export Base.Codec.Result

  final type Collection[A] = Base.Collection[Data.Optional, Data, A]

  object Collection:
    type Of[O <: Data, A] = Base.Collection[Data.Optional, O, A]

    type Required[A] = Base.Collection[Data.Required, Data, A]

    object Required:
      type Of[O <: Data, A] = Base.Collection[Data.Required, O, A]

  final type Dictionary[A] = Base.Dictionary[Data.Optional, Data, A]

  object Dictionary:
    type Of[O <: Data, A] = Base.Dictionary[Data.Optional, O, A]

    type Required[A] = Base.Dictionary[Data.Required, Data, A]

    object Required:
      type Of[O <: Data, A] = Base.Dictionary[Data.Required, O, A]

  final type Dynamic[A] = Base.Dynamic[Data.Optional, Data, A]

  object Dynamic:
    type Of[O <: Data, A] = Base.Dynamic[Data.Optional, O, A]

    type Required[A] = Base.Dynamic[Data.Required, Data, A]

    object Required:
      type Of[O <: Data, A] = Base.Dynamic[Data.Required, O, A]

  final type Enumeration[A] = Base.Enumeration[Data.Optional, A]

  object Enumeration:
    type Required[A] = Base.Enumeration[Data.Required, A]

  final type Primitive[A] = Base.Primitive[Data.Optional, A]

  object Primitive:
    type Required[A] = Base.Primitive[Data.Required, A]

  final type Product[A] = Base.Product[Data.Optional, ?, Data, A]

  object Product:
    type Of[O <: Data, A] = Base.Product[Data.Optional, ?, O, A]

    type Required[A] = Base.Product[Data.Required, ?, Data, A]

    object Required:
      type Of[O <: Data, A] = Base.Product[Data.Required, ?, O, A]

  type Record[A] = Base.Record[Data.Optional, Data, A]

  object Record:
    type Of[O <: Data, A] = Base.Record[Data.Optional, O, A]

    type Required[A] = Base.Record[Data.Required, Data, A]

    object Required:
      type Of[O <: Data, A] = Base.Record[Data.Required, O, A]

  final type Sum[A] = Base.Sum[Data.Optional, Data, A]

  object Sum:
    type Of[O <: Data, A] = Base.Sum[Data.Optional, O, A]

    type Required[A] = Base.Sum[Data.Required, Data, A]

    object Required:
      type Of[O <: Data, A] = Base.Sum[Data.Required, O, A]

    type Nested[A] = Base.Sum.Nested[Data.Optional, Data, A]

    object Nested:
      type Of[O <: Data, A] = Base.Sum.Nested[Data.Optional, O, A]

      type Required[A] = Base.Sum.Nested[Data.Required, Data, A]

      object Required:
        type Of[O <: Data, A] = Base.Sum.Nested[Data.Required, O, A]

    type Merged[A] = Base.Sum.Merged[Data.Optional, Data, A]

    object Merged:
      type Of[O <: Data, A] = Base.Sum.Merged[Data.Optional, O, A]

      type Required[O <: Data, A] = Base.Sum.Merged[Data.Required, O, A]

      object Required:
        type Of[O <: Data, A] = Base.Sum.Merged[Data.Required, O, A]

    type Keyed[A] = Base.Sum.Merged[Data.Optional, Data, A]

    object Keyed:
      type Of[O <: Data, A] = Base.Sum.Keyed[Data.Optional, O, A]

      type Required[O <: Data, A] = Base.Sum.Keyed[Data.Required, O, A]

      object Required:
        type Of[O <: Data, A] = Base.Sum.Keyed[Data.Required, O, A]

    type Untagged[A] = Base.Sum.Untagged[Data.Optional, Data, A]

    object Untagged:
      type Of[O <: Data, A] = Base.Sum.Untagged[Data.Optional, O, A]

      type Required[O <: Data, A] = Base.Sum.Untagged[Data.Required, O, A]

      object Required:
        type Of[O <: Data, A] = Base.Sum.Untagged[Data.Required, O, A]

  type Tuple[A] = Base.Tuple[Data.Optional, Data, A]

  object Tuple:
    type Of[O <: Data, A] = Base.Tuple[Data.Optional, O, A]

    type Required[A] = Base.Tuple[Data.Required, Data, A]

    object Required:
      type Of[O <: Data, A] = Base.Tuple[Data.Required, O, A]

  final type Branch[A] = Base.Branch[Data, A]

  object Branch:
    type Of[O <: Data, A] = Base.Branch[O, A]

  final type Field[A] = Base.Field[Data, A]

  object Field:
    type Of[O <: Data, A] = Base.Field[O, A]

object Types extends Types
