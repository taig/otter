package io.taig.otter

import io.taig.otter as Base
import cats.Id as Identity

trait Types:
  export Base.{CodecValidation, Constraint, Metadata, Type}

  final type Codec[A] = Base.Codec[Data.Optional, ?, A]

  object Codec:
    type Of[O <: Data.Value, A] = Base.Codec[Data.Optional, O, A]

    type Required[A] = Base.Codec[Identity, ?, A]

    object Required:
      type Of[O <: Data.Value, A] = Base.Codec[Identity, O, A]

    export Base.Codec.Result

  // // final type Value[A] = Base.Value[?, A]

  // // object Value:
  // //   type Of[O, A] = Base.Value[O, A]

  // //   type Required[A] = Base.Value.Required[?, A]

  // //   object Required:
  // //     type Of[O, A] = Base.Value.Required[O, A]

  final type Collection[A] = Base.Collection[Data.Optional, ?, A]

  object Collection:
    type Of[O <: Data, A] = Base.Collection[Data.Optional, O, A]

    type Required[A] = Base.Collection[Identity, ?, A]

    object Required:
      type Of[O <: Data, A] = Base.Collection[Identity, O, A]

  // // final type Dictionary[A] = Base.Dictionary[?, A]

  // // object Dictionary:
  // //   type Of[O, A] = Base.Dictionary[O, A]

  // // final type Dynamic[A] = Base.Dynamic[A]

  // // final type Enumeration[A] = Base.Enumeration[?, A]

  // // object Enumeration:
  // //   type Of[O, A] = Base.Enumeration[O, A]

  // //   type Required[A] = Base.Enumeration.Required[?, A]

  // //   object Required:
  // //     type Of[O, A] = Base.Enumeration.Required[O, A]

  final type Primitive[A] = Base.Primitive[Data.Optional, A]

  object Primitive:
    type Required[A] = Base.Primitive[Identity, A]

  // final type Product[A] = Base.Product[?, A]

  // object Product:
  //   type Of[O, A] = Base.Product[O, A]

  type Record[A] = Base.Record[Data.Optional, ?, A]

  object Record:
    type Of[O <: Data, A] = Base.Record[Data.Optional, O, A]

    type Required[A] = Base.Record[Identity, ?, A]

    object Required:
      type Of[O <: Data, A] = Base.Record[Identity, O, A]

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

  type Tuple[A] = Base.Tuple[Data.Optional, ?, A]

  object Tuple:
    type Of[O <: Data, A] = Base.Tuple[Data.Optional, O, A]

    type Required[A] = Base.Tuple[Identity, ?, A]

    object Required:
      type Of[O <: Data, A] = Base.Tuple[Identity, O, A]

  // final type Branch[A] = Base.Branch[?, A]

  // object Branch:
  //   type Of[O, A] = Base.Branch[O, A]

  final type Field[A] = Base.Field[?, A]

  object Field:
    type Of[O <: Data, A] = Base.Field[O, A]

object Types extends Types
