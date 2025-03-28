package io.taig.otter

import io.taig.otter as Base

trait Types:
  export Base.{
// Attribute,
    Comparison,
// Constraint,
// Data,
// Discriminator,
    Convert,
    Data,
    Merge,
    Metadata
  }

  final type Codec[A] = Base.Codec[Data.Any, A]

  object Codec:
    type Of[F <: Data.Any, A] = Base.Codec[F, A]

  final type Collection[A] = Base.Collection[Data.Any, A]

  object Collection:
    type Of[F <: Data.Any, A] = Base.Collection[F, A]

  final type Constant[A] = Base.Constant[Data.Primitive, A]

  object Constant:
    type Of[F <: Data.Primitive, A] = Base.Constant[F, A]

  final type Dictionary[A] = Base.Dictionary[Data.Any, A]

  object Dictionary:
    type Of[F <: Data.Any, A] = Base.Dictionary[F, A]

  final type Enumeration[A] = Base.Enumeration[Data.Primitive, A]

  object Enumeration:
    type Of[F <: Data.Primitive, A] = Base.Enumeration[F, A]

  final type Optional[A] = Base.Optional[Data.Any, A]

  object Optional:
    type Of[F <: Data.Any, A] = Base.Optional[F, A]

  final type Primitive[A] = Base.Primitive[Data.Primitive, A]

  object Primitive:
    type Of[F <: Data.Primitive, A] = Base.Primitive[F, A]

  final type Record[A] = Base.Record[Data.Any, A]

  object Record:
    type Of[F <: Data.Any, A] = Base.Record[F, A]

  // final type Union[A] = Base.Union[Data.Any, A]

  // object Union:
  //   type Of[F <: Data.Any, A] = Base.Union[F, A]

  //   type Tagged[A] = Base.Union[Data.Object[?], A]

  //   object Tagged:
  //     type Of[F <: Data.Any, A] = Base.Union[Data.Object[F], A]

//     type Nested[A] = Tagged[A]

//     object Nested:
//       type Of[O <: Data, A] = Tagged.Of[String | O, A]

//     type Merged[A] = Tagged[A]

//     object Merged:
//       type Of[O <: Data, A] = Tagged.Of[String | O, A]

//     type Keyed[A] = Tagged[A]

//     object Keyed:
//       type Of[O <: Data, A] = Tagged.Of[O, A]

  type Tuple[A] = Base.Tuple[Data.Any, A]

  object Tuple:
    type Of[F <: Data.Any, A] = Base.Tuple[F, A]

  final type Branch[A] = Base.Branch[Data.Any, A]

  object Branch:
    type Of[F <: Data.Any, A] = Base.Branch[F, A]

    type Tagged[A] = Base.Branch[Data.Object[?], A]

    object Tagged:
      type Of[F <: Data.Any, A] = Base.Branch[Data.Object[F], A]

    type Nested[A] = Tagged[A]

    object Nested:
      type Of[F <: Data.Any, A] = Tagged.Of[String | F, A]

    type Merged[A] = Tagged[A]

    object Merged:
      type Of[F <: Data.Any, A] = Tagged.Of[String | F, A]

    type Keyed[A] = Tagged[A]

    object Keyed:
      type Of[F <: Data.Any, A] = Tagged.Of[F, A]

  final type Field[A] = Base.Field[Data.Any, A]

  object Field:
    type Of[F <: Data.Any, A] = Base.Field[F, A]

    type Required[A] = Base.Field.Required[Data.Any, A]

    object Required:
      type Of[F <: Data.Any, A] = Base.Field.Required[F, A]

object Types extends Types
