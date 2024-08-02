package io.taig.otter

import io.taig.otter as Base

trait Types:
  export Base.{CodecValidation, Constraint, Data, Metadata, Type}
  export validation.{Step, Validation, Violation, Violations}

  // final type Codec[A] = Base.Codec.Of[Data.Optional, Data, A]

  // object Codec:
  //   type Of[O <: Data, A] = Base.Codec.Of[Data.Optional, O, A]

  //   type Required[A] = Base.Codec.Of[Data.Required, Data, A]

  //   object Required:
  //     type Of[O <: Data, A] = Base.Codec.Of[Data.Required, O, A]

  //   export Base.Codec.Result

  // final type Collection[A] = Base.Collection.Of[Data.Optional, Data, A]

  // object Collection:
  //   type Of[O <: Data, A] = Base.Collection.Of[Data.Optional, O, A]

  //   type Required[A] = Base.Collection.Of[Data.Required, Data, A]

  //   object Required:
  //     type Of[O <: Data, A] = Base.Collection.Of[Data.Required, O, A]

  // final type Dictionary[A] = Base.Dictionary.Of[Data.Optional, Data, A]

  // object Dictionary:
  //   type Of[O <: Data, A] = Base.Dictionary.Of[Data.Optional, O, A]

  //   type Required[A] = Base.Dictionary.Of[Data.Required, Data, A]

  //   object Required:
  //     type Of[O <: Data, A] = Base.Dictionary.Of[Data.Required, O, A]

  // final type Dynamic[A] = Base.Dynamic.Of[Data.Optional, Data, A]

  // object Dynamic:
  //   type Of[O <: Data, A] = Base.Dynamic.Of[Data.Optional, O, A]

  //   type Required[A] = Base.Dynamic.Of[Data.Required, Data, A]

  //   object Required:
  //     type Of[O <: Data, A] = Base.Dynamic.Of[Data.Required, O, A]

  // final type Enumeration[A] = Base.Enumeration.Of[Data.Optional, A]

  // object Enumeration:
  //   type Required[A] = Base.Enumeration.Of[Data.Required, A]

  // final type Primitive[A] = Base.Primitive.Of[Data.Optional, A]

  // object Primitive:
  //   type Required[A] = Base.Primitive.Of[Data.Required, A]

  // final type Product[A] = Base.Product.Of[Data.Optional, Data.Object[?] | Data.Array[?], A]

  // object Product:
  //   type Of[O <: Data, A] = Base.Product.Of[Data.Optional, Data.Object[O] | Data.Array[O], A]

  //   type Required[A] = Base.Product.Of[Data.Required, Data.Object[?] | Data.Array[?], A]

  //   object Required:
  //     type Of[O <: Data, A] = Base.Product.Of[Data.Required, Data.Object[O] | Data.Array[O], A]

  // type Record[A] = Base.Record.Of[Data.Optional, Data, A]

  // object Record:
  //   type Of[O <: Data, A] = Base.Record.Of[Data.Optional, O, A]

  //   type Required[A] = Base.Record.Of[Data.Required, Data, A]

  //   object Required:
  //     type Of[O <: Data, A] = Base.Record.Of[Data.Required, O, A]

  // final type Sum[A] = Base.Sum[Data.Optional, ?, A]

  // object Sum:
  //   type Of[O <: Data, A] = Base.Sum[Data.Optional, O, A]

  //   type Required[A] = Base.Sum[Data.Required, ?, A]

  //   object Required:
  //     type Of[O <: Data, A] = Base.Sum[Data.Required, O, A]

  //   type Nested[A] = Base.Sum.Nested[Data.Optional, ?, A]

  //   object Nested:
  //     type Of[O <: Data, A] = Base.Sum.Nested[Data.Optional, O, A]

  //     type Required[A] = Base.Sum.Nested[Data.Required, ?, A]

  //     object Required:
  //       type Of[O <: Data, A] = Base.Sum.Nested[Data.Required, O, A]

  //   type Merged[A] = Base.Sum.Merged[Data.Optional, ?, A]

  //   object Merged:
  //     type Of[O <: Data, A] = Base.Sum.Merged[Data.Optional, O, A]

  //     type Required[O <: Data, A] = Base.Sum.Merged[Data.Required, O, A]

  //     object Required:
  //       type Of[O <: Data, A] = Base.Sum.Merged[Data.Required, O, A]

  //   type Keyed[A] = Base.Sum.Merged[Data.Optional, ?, A]

  //   object Keyed:
  //     type Of[O <: Data, A] = Base.Sum.Keyed[Data.Optional, O, A]

  //     type Required[O <: Data, A] = Base.Sum.Keyed[Data.Required, O, A]

  //     object Required:
  //       type Of[O <: Data, A] = Base.Sum.Keyed[Data.Required, O, A]

  //   type Untagged[A] = Base.Sum.Untagged[Data.Optional, ?, A]

  //   object Untagged:
  //     type Of[O <: Data, A] = Base.Sum.Untagged[Data.Optional, O, A]

  //     type Required[O <: Data, A] = Base.Sum.Untagged[Data.Required, O, A]

  //     object Required:
  //       type Of[O <: Data, A] = Base.Sum.Untagged[Data.Required, O, A]

  // type Tuple[A] = Base.Tuple.Of[Data.Optional, Data, A]

  // object Tuple:
  //   type Of[O <: Data, A] = Base.Tuple.Of[Data.Optional, O, A]

  //   type Required[A] = Base.Tuple.Of[Data.Required, Data, A]

  //   object Required:
  //     type Of[O <: Data, A] = Base.Tuple.Of[Data.Required, O, A]

  // final type Branch[A] = Base.Branch[?, A]

  // object Branch:
  //   type Of[O <: Data, A] = Base.Branch[O, A]

  // final type Field[A] = Base.Field[?, A]

  // object Field:
  //   type Of[O <: Data, A] = Base.Field[O, A]

object Types extends Types
