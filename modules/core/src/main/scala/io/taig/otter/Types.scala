package io.taig.otter

import io.taig.otter as Base

trait Types:
  export Base.{Attribute, Comparison, Constraint, Convert, Data, Discriminator, Merge, Metadata}

//   final type Codec[A] = Base.Codec[Data.Any, A]

//   object Codec:
//     type Of[S <: Data.Any, A] = Base.Codec[S, A]

//   final type Collection[A] = Base.Collection[Data.Any, A]

//   object Collection:
//     type Of[S <: Data.Any, A] = Base.Collection[S, A]

//   final type Constant[A] = Base.Constant[Data.Any, A]

//   object Constant:
//     type Of[S <: Data.Any, A] = Base.Constant[S, A]

//   final type Dictionary[A] = Base.Dictionary[Data.Any, A]

//   object Dictionary:
//     type Of[S <: Data.Any, A] = Base.Dictionary[S, A]

//   final type Enumeration[A] = Base.Enumeration[Data.Primitive, A]

//   object Enumeration:
//     type Of[S <: Data.Primitive, A] = Base.Enumeration[S, A]

//   final type Optional[A] = Base.Optional[Data.Any, A]

//   object Optional:
//     type Of[S <: Data.Any, A] = Base.Optional[S, A]

//   final type Primitive[A] = Base.Primitive[Data.Primitive, A]

//   object Primitive:
//     type Of[S <: Data.Primitive, A] = Base.Primitive[S, A]

//   final type Record[A] = Base.Record[Data.Any, A]

//   object Record:
//     type Of[S <: Data.Any, A] = Base.Record[S, A]

//   final type Union[A] = Base.Union[?, Data.Any, A]

//   object Union:
//     type Of[S <: Data.Any, A] = Base.Union[?, S, A]

//     type Untagged[A] = Base.Union.Untagged[Data.Any, A]

//     object Untagged:
//       type Of[S <: Data.Any, A] = Base.Union.Untagged[S, A]

//     type Tagged[A] = Base.Union.Tagged[?, Data.Any, A]

//     object Tagged:
//       type Of[S <: Data.Any, A] = Base.Union.Tagged[?, S, A]

//       // TODO Keyed, Merged, ...

//   final type Tuple[A] = Base.Tuple[Data.Any, A]

//   object Tuple:
//     type Of[S <: Data.Any, A] = Base.Tuple[S, A]

//   final type Branch[A] = Base.Branch[Data.Any, A]

//   object Branch:
//     type Of[S <: Data.Any, A] = Base.Branch[S, A]

//   final type Sield[A] = Base.Field[Data.Any, A]

//   object Sield:
//     type Of[S <: Data.Any, A] = Base.Field[S, A]

//     type Required[A] = Base.Field.Required[Data.Any, A]

//     object Required:
//       type Of[S <: Data.Any, A] = Base.Field.Required[S, A]

// object Types extends Types
