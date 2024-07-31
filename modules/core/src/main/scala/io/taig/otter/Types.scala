package io.taig.otter

import io.taig.otter as Base
import cats.Id as Identity

trait Types:
  export Base.{CodecValidation, Constraint, Data, Metadata, Type}

  final type Codec[A] = Base.Codec[Data.Optional, ?, A]

  object Codec:
    type Of[O <: Data.Value, A] = Base.Codec[Data.Optional, O, A]

    type Required[A] = Base.Codec[Identity, ?, A]

    object Required:
      type Of[O <: Data.Value, A] = Base.Codec[Identity, O, A]

    export Base.Codec.Result

  final type Collection[A] = Base.Collection[Data.Optional, ?, A]

  object Collection:
    type Of[O <: Data, A] = Base.Collection[Data.Optional, O, A]

    type Required[A] = Base.Collection[Identity, ?, A]

    object Required:
      type Of[O <: Data, A] = Base.Collection[Identity, O, A]

  final type Dictionary[A] = Base.Dictionary[Data.Optional, ?, A]

  object Dictionary:
    type Of[O <: Data, A] = Base.Dictionary[Data.Optional, O, A]

    type Required[A] = Base.Dictionary[Identity, ?, A]

    object Required:
      type Of[O <: Data, A] = Base.Dictionary[Identity, O, A]

  final type Dynamic[A] = Base.Dynamic[Data.Optional, ?, A]

  final type Enumeration[A] = Base.Enumeration[Data.Optional, A]

  object Enumeration:
    type Required[A] = Base.Enumeration[Identity, A]

  final type Primitive[A] = Base.Primitive[Data.Optional, A]

  object Primitive:
    type Required[A] = Base.Primitive[Identity, A]

  final type Product[A] = Base.Product[Data.Optional, ?, A]

  object Product:
    type Of[O <: Data, A] = Base.Product[Data.Optional, Data.Array[O] | Data.Object[O], A]

    type Required[A] = Base.Product[Identity, ?, A]

    object Required:
      type Of[O <: Data, A] = Base.Product[Identity, Data.Array[O] | Data.Object[O], A]

  type Record[A] = Base.Record[Data.Optional, ?, A]

  object Record:
    type Of[O <: Data, A] = Base.Record[Data.Optional, O, A]

    type Required[A] = Base.Record[Identity, ?, A]

    object Required:
      type Of[O <: Data, A] = Base.Record[Identity, O, A]

  final type Sum[A] = Base.Sum[Data.Optional, ?, A]

  object Sum:
    type Of[O <: Data, A] = Base.Sum[Data.Optional, O, A]

    type Required[A] = Base.Sum[Identity, ?, A]

    object Required:
      type Of[O <: Data, A] = Base.Sum[Identity, O, A]

    type Nested[A] = Base.Sum.Nested[Data.Optional, ?, A]

    object Nested:
      type Of[O <: Data, A] = Base.Sum.Nested[Data.Optional, O, A]

      type Required[O <: Data, A] = Base.Sum.Nested[Identity, O, A]

      object Required:
        type Of[O <: Data, A] = Base.Sum.Nested[Identity, O, A]

    type Merged[A] = Base.Sum.Merged[Data.Optional, ?, A]

    object Merged:
      type Of[O <: Data, A] = Base.Sum.Merged[Data.Optional, O, A]

      type Required[O <: Data, A] = Base.Sum.Merged[Identity, O, A]

      object Required:
        type Of[O <: Data, A] = Base.Sum.Merged[Identity, O, A]

    type Keyed[A] = Base.Sum.Merged[Data.Optional, ?, A]

    object Keyed:
      type Of[O <: Data, A] = Base.Sum.Keyed[Data.Optional, O, A]

      type Required[O <: Data, A] = Base.Sum.Keyed[Identity, O, A]

      object Required:
        type Of[O <: Data, A] = Base.Sum.Keyed[Identity, O, A]

    type Untagged[A] = Base.Sum.Untagged[Data.Optional, ?, A]

    object Untagged:
      type Of[O <: Data, A] = Base.Sum.Untagged[Data.Optional, O, A]

      type Required[O <: Data, A] = Base.Sum.Untagged[Identity, O, A]

      object Required:
        type Of[O <: Data, A] = Base.Sum.Untagged[Identity, O, A]

  type Tuple[A] = Base.Tuple[Data.Optional, ?, A]

  object Tuple:
    type Of[O <: Data, A] = Base.Tuple[Data.Optional, O, A]

    type Required[A] = Base.Tuple[Identity, ?, A]

    object Required:
      type Of[O <: Data, A] = Base.Tuple[Identity, O, A]

  final type Branch[A] = Base.Branch[?, A]

  object Branch:
    type Of[O <: Data, A] = Base.Branch[O, A]

  final type Field[A] = Base.Field[?, A]

  object Field:
    type Of[O <: Data, A] = Base.Field[O, A]

object Types extends Types
