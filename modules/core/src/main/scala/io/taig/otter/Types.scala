package io.taig.otter

import io.taig.otter as Base

trait Types:
  export Base.{
    Attribute,
    Comparison,
    Constraint,
    Data,
    Discriminator,
    Merge,
    Metadata,
    Null,
    Step,
    Violation,
    Violations
  }

  final type Codec[A] = Base.Codec[Data, A]

  object Codec:
    type Of[O <: Data, A] = Base.Codec[O, A]

    export Base.Codec.Result

  final type Collection[A] = Base.Collection[Data, A]

  object Collection:
    type Of[O <: Data, A] = Base.Collection[O, A]

  final type Constant[A] = Base.Constant[Data.Primitive, A]

  object Constant:
    type Of[O <: Data.Primitive, A] = Base.Constant[O, A]

  final type Dictionary[A] = Base.Dictionary[Data, A]

  object Dictionary:
    type Of[O <: Data, A] = Base.Dictionary[O, A]

  final type Dynamic[A] = Base.Dynamic[Data, A]

  object Dynamic:
    type Of[O <: Data, A] = Base.Dynamic[O, A]

  final type Enumeration[A] = Base.Enumeration[Data.Primitive, A]

  object Enumeration:
    type Of[O <: Data.Primitive, A] = Base.Enumeration[O, A]

  final type Primitive[A] = Base.Primitive[Data.Primitive, A]

  object Primitive:
    type Of[O <: Data.Primitive, A] = Base.Primitive[O, A]

  final type Record[A] = Base.Record[Data, A]

  object Record:
    type Of[O <: Data, A] = Base.Record[O, A]

  final type Union[A] = Base.Union[Data, A]

  object Union:
    type Of[O <: Data, A] = Base.Union[O, A]

  type Tuple[A] = Base.Tuple[Data, A]

  object Tuple:
    type Of[O <: Data, A] = Base.Tuple[O, A]

  final type Branch[A] = Base.Branch[Data, A]

  object Branch:
    type Of[O <: Data, A] = Base.Branch[O, A]

    type Tagged[A] = Base.Branch[Data.Object[?], A]

    object Tagged:
      type Of[O <: Data, A] = Base.Branch[Data.Object[O], A]

    type Nested[A] = Tagged[A]

    object Nested:
      type Of[O <: Data, A] = Tagged.Of[Data.Primitive | O, A]

    type Merged[A] = Tagged[A]

    object Merged:
      type Of[O <: Data, A] = Tagged.Of[Data.Primitive | O, A]

    type Keyed[A] = Tagged[A]

    object Keyed:
      type Of[O <: Data, A] = Tagged.Of[O, A]

  final type Field[A] = Base.Field[Data, A]

  object Field:
    type Of[O <: Data, A] = Base.Field[O, A]

    type Required[A] = Base.Field.Required[Data.Value, A]

    object Required:
      type Of[O <: Data.Value, A] = Base.Field.Required[O, A]

object Types extends Types
