package io.taig.otter

import io.taig.otter as Base

trait Types:
  export Base.{CodecValidation, Constraint, Metadata, Type}

  final type Codec[A] = Base.Codec[?, A]

  object Codec:
    type Of[O <: Data[?], A] = Base.Codec[O, A]

    type Required[A] = Base.Codec[Data.Value[?], A]

    object Required:
      type Of[O <: Data.Value[?], A] = Base.Codec[O, A]

    export Base.Codec.Result

  // final type Value[A] = Base.Value[?, A]

  // object Value:
  //   type Of[O, A] = Base.Value[O, A]

  //   type Required[A] = Base.Value.Required[?, A]

  //   object Required:
  //     type Of[O, A] = Base.Value.Required[O, A]

  final type Collection[A] = Base.Collection[?, A]

  object Collection:
    type Of[O <: Data[?], A] = Base.Collection[Data.Optional[Data.Array[O]], A]

    type Required[A] = Base.Collection[Data.Array[?], A]

    object Required:
      type Of[O <: Data[?], A] = Base.Collection[Data.Array[O], A]

  // final type Dictionary[A] = Base.Dictionary[?, A]

  // object Dictionary:
  //   type Of[O, A] = Base.Dictionary[O, A]

  // final type Dynamic[A] = Base.Dynamic[A]

  // final type Enumeration[A] = Base.Enumeration[?, A]

  // object Enumeration:
  //   type Of[O, A] = Base.Enumeration[O, A]

  //   type Required[A] = Base.Enumeration.Required[?, A]

  //   object Required:
  //     type Of[O, A] = Base.Enumeration.Required[O, A]

  final type Primitive[A] = Base.Primitive[?, A]

  object Primitive:
    type Required[A] = Base.Primitive[Data.Primitive, A]

  // final type Product[A] = Base.Product[?, A]

  // object Product:
  //   type Of[O, A] = Base.Product[O, A]

  // final type Record[A] = Base.Record[?, A]

  // object Record:
  //   type Of[O, A] = Base.Record[O, A]

  // final type Sum[A] = Base.Sum[?, A]

  // object Sum:
  //   type Of[O, A] = Base.Sum[O, A]

  // final type Union[A] = Base.Union[?, A]

  // object Union:
  //   type Of[O, A] = Base.Union[O, A]

  //   type Value[A] = Base.Union.Value[?, A]

  //   object Value:
  //     type Of[O, A] = Base.Union.Value[O, A]

  //     type Required[A] = Base.Union.Value.Required[?, A]

  //     object Required:
  //       type Of[O, A] = Base.Union.Value.Required[O, A]

  // final type Branch[A] = Base.Branch[?, A]

  // object Branch:
  //   type Of[O, A] = Base.Branch[O, A]

  // final type Field[A] = Base.Field[?, A]

  // object Field:
  //   type Of[O, A] = Base.Field[O, A]

object Types extends Types
