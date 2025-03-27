package io.taig.otter

import io.taig.otter as Base

trait Types:
  export Base.{
// Attribute,
    Comparison,
    Format,
// Constraint,
// Format,
// Discriminator,
// Merge,
    Metadata
  }

  final type Codec[A] = Base.Codec[Format.Any, A]

  object Codec:
    type Of[F <: Format.Any, A] = Base.Codec[F, A]

  final type Collection[A] = Base.Collection[Format.Any, A]

  object Collection:
    type Of[F <: Format.Any, A] = Base.Collection[F, A]

  final type Constant[A] = Base.Constant[Format.Primitive, A]

  object Constant:
    type Of[F <: Format.Primitive, A] = Base.Constant[F, A]

  final type Dictionary[A] = Base.Dictionary[Format.Any, A]

  object Dictionary:
    type Of[F <: Format.Any, A] = Base.Dictionary[F, A]

  final type Enumeration[A] = Base.Enumeration[Format.Primitive, A]

  object Enumeration:
    type Of[F <: Format.Primitive, A] = Base.Enumeration[F, A]

  final type Optional[A] = Base.Optional[Format.Any, A]

  object Optional:
    type Of[F <: Format.Any, A] = Base.Optional[F, A]

  final type Primitive[A] = Base.Primitive[Format.Primitive, A]

  object Primitive:
    type Of[F <: Format.Primitive, A] = Base.Primitive[F, A]

  final type Record[A] = Base.Record[Format.Any, A]

  object Record:
    type Of[F <: Format.Any, A] = Base.Record[F, A]

  final type Union[A] = Base.Union[Format.Any, A]

  object Union:
    type Of[F <: Format.Any, A] = Base.Union[F, A]

    type Tagged[A] = Base.Union[Format.Object[?], A]

    object Tagged:
      type Of[F <: Format.Any, A] = Base.Union[Format.Object[F], A]

//     type Nested[A] = Tagged[A]

//     object Nested:
//       type Of[O <: Format, A] = Tagged.Of[Format.String | O, A]

//     type Merged[A] = Tagged[A]

//     object Merged:
//       type Of[O <: Format, A] = Tagged.Of[Format.String | O, A]

//     type Keyed[A] = Tagged[A]

//     object Keyed:
//       type Of[O <: Format, A] = Tagged.Of[O, A]

  type Tuple[A] = Base.Tuple[Format.Any, A]

  object Tuple:
    type Of[F <: Format.Any, A] = Base.Tuple[F, A]

  final type Branch[A] = Base.Branch[Format.Any, A]

  object Branch:
    type Of[F <: Format.Any, A] = Base.Branch[F, A]

    type Tagged[A] = Base.Branch[Format.Object[?], A]

    object Tagged:
      type Of[F <: Format.Any, A] = Base.Branch[Format.Object[F], A]

    type Nested[A] = Tagged[A]

    object Nested:
      type Of[F <: Format.Any, A] = Tagged.Of[Format.String | F, A]

    type Merged[A] = Tagged[A]

    object Merged:
      type Of[F <: Format.Any, A] = Tagged.Of[Format.String | F, A]

    type Keyed[A] = Tagged[A]

    object Keyed:
      type Of[F <: Format.Any, A] = Tagged.Of[F, A]

  final type Field[A] = Base.Field[Format.Any, A]

  object Field:
    type Of[F <: Format.Any, A] = Base.Field[F, A]

    type Required[A] = Base.Field.Required[Format.Any, A]

    object Required:
      type Of[F <: Format.Any, A] = Base.Field.Required[F, A]

object Types extends Types
