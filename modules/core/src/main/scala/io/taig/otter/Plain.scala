package io.taig.otter

import io.taig.otter as Base
import cats.Id as Identity
import cats.Applicative
import cats.Comonad

trait Plain extends Dsl
// final type Schema[A] = Base.Schema[Identity, Any, ?, A]

// object Schema:
//   type Of[A, B] = Base.Schema[Identity, Any, A, B]
//   type Via[A, B] = Base.Schema[Identity, A, ?, B]
//   type With[A, B, C] = Base.Schema[Identity, A, B, C]

//   type Reader[A] = Base.Schema.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Schema.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Schema.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Schema.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Schema.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Schema.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Schema.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Schema.Writer[Identity, A, B, C]

// final type Value[A] = Base.Value[Identity, Any, ?, A]

// object Value:
//   type Of[A, B] = Base.Value[Identity, Any, A, B]
//   type Via[A, B] = Base.Value[Identity, A, ?, B]
//   type With[A, B, C] = Base.Value[Identity, A, B, C]

//   type Required[A] = Base.Value.Required[Identity, Any, ?, A]

//   object Required:
//     type Of[A, B] = Base.Value.Required[Identity, Any, A, B]
//     type Via[A, B] = Base.Value.Required[Identity, A, ?, B]
//     type With[A, B, C] = Base.Value.Required[Identity, A, B, C]

//     type Reader[A] = Base.Value.Required.Reader[Identity, Any, ?, A]

//     object Reader:
//       type Of[A, B] = Base.Value.Required.Reader[Identity, Any, A, B]
//       type Via[A, B] = Base.Value.Required.Reader[Identity, A, ?, B]
//       type With[A, B, C] = Base.Value.Required.Reader[Identity, A, B, C]

//     type Writer[A] = Base.Value.Required.Writer[Identity, Any, ?, A]

//     object Writer:
//       type Of[A, B] = Base.Value.Required.Writer[Identity, Any, A, B]
//       type Via[A, B] = Base.Value.Required.Writer[Identity, A, ?, B]
//       type With[A, B, C] = Base.Value.Required.Writer[Identity, A, B, C]

//   type Reader[A] = Base.Value.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Value.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Value.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Value.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Value.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Value.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Value.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Value.Writer[Identity, A, B, C]

// final type Collection[A] = Base.Collection[Identity, Any, ?, A]

// object Collection:
//   type Of[A, B] = Base.Collection[Identity, Any, A, B]
//   type Via[A, B] = Base.Collection[Identity, A, ?, B]
//   type With[A, B, C] = Base.Collection[Identity, A, B, C]

//   type Reader[A] = Base.Collection.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Collection.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Collection.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Collection.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Collection.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Collection.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Collection.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Collection.Writer[Identity, A, B, C]

// final type Dictionary[A] = Base.Dictionary[Identity, Any, ?, A]

// object Dictionary:
//   type Of[A, B] = Base.Dictionary[Identity, Any, A, B]
//   type Via[A, B] = Base.Dictionary[Identity, A, ?, B]
//   type With[A, B, C] = Base.Dictionary[Identity, A, B, C]

//   type Reader[A] = Base.Dictionary.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Dictionary.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Dictionary.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Dictionary.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Dictionary.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Dictionary.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Dictionary.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Dictionary.Writer[Identity, A, B, C]

// final type Dynamic[A, B] = Base.Dynamic[A, B]

// object Dynamic:
//   type Reader[A, B] = Base.Dynamic.Reader[A, B]

//   type Writer[A, B] = Base.Dynamic.Writer[A, B]

// final type Enumeration[A] = Base.Enumeration[Identity, Any, ?, A]

// object Enumeration:
//   type Of[A, B] = Base.Enumeration.Required[Identity, Any, A, B]
//   type Via[A, B] = Base.Enumeration.Required[Identity, A, ?, B]
//   type With[A, B, C] = Base.Enumeration.Required[Identity, A, B, C]

//   type Required[A] = Base.Enumeration.Required[Identity, Any, ?, A]

//   object Required:
//     type Of[A, B] = Base.Enumeration.Required[Identity, Any, A, B]
//     type Via[A, B] = Base.Enumeration.Required[Identity, A, ?, B]
//     type With[A, B, C] = Base.Enumeration.Required[Identity, A, B, C]

//     type Reader[A] = Base.Enumeration.Required.Reader[Identity, Any, ?, A]

//     object Reader:
//       type Of[A, B] = Base.Enumeration.Required.Reader[Identity, Any, A, B]
//       type Via[A, B] = Base.Enumeration.Required.Reader[Identity, A, ?, B]
//       type With[A, B, C] = Base.Enumeration.Required.Reader[Identity, A, B, C]

//     type Writer[A] = Base.Enumeration.Required.Writer[Identity, Any, ?, A]

//     object Writer:
//       type Of[A, B] = Base.Enumeration.Required.Writer[Identity, Any, A, B]
//       type Via[A, B] = Base.Enumeration.Required.Writer[Identity, A, ?, B]
//       type With[A, B, C] = Base.Enumeration.Required.Writer[Identity, A, B, C]

//   type Reader[A] = Base.Enumeration.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Enumeration.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Enumeration.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Enumeration.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Enumeration.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Enumeration.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Enumeration.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Enumeration.Writer[Identity, A, B, C]

// final type Primitive[A] = Base.Primitive[A]

// object Primitive:

//   type Required[A] = Base.Primitive.Required[A]

//   object Required:
//     type Reader[A] = Base.Primitive.Required.Reader[A]

//     type Writer[A] = Base.Primitive.Required.Writer[A]

//   type Reader[A] = Base.Primitive.Reader[A]

//   type Writer[A] = Base.Primitive.Writer[A]

// final type Product[A] = Base.Product[Identity, Any, ?, A]

// object Product:
//   type Of[A, B] = Base.Product[Identity, Any, A, B]
//   type Via[A, B] = Base.Product[Identity, A, ?, B]
//   type With[A, B, C] = Base.Product[Identity, A, B, C]

//   type Reader[A] = Base.Product.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Product.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Product.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Product.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Product.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Product.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Product.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Product.Writer[Identity, A, B, C]

// final type Record[A] = Base.Record[Identity, Any, ?, A]

// object Record:
//   type Of[A, B] = Base.Record[Identity, Any, A, B]
//   type Via[A, B] = Base.Record[Identity, A, ?, B]
//   type With[A, B, C] = Base.Record[Identity, A, B, C]

//   type Reader[A] = Base.Record.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Record.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Record.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Record.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Record.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Record.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Record.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Record.Writer[Identity, A, B, C]

//   export Base.Record.Null

// final type Sum[A] = Base.Sum[Identity, Any, ?, A]

// object Sum:
//   export Base.Sum.Discriminator

//   type Of[A, B] = Base.Sum[Identity, Any, A, B]
//   type Via[A, B] = Base.Sum[Identity, A, ?, B]
//   type With[A, B, C] = Base.Sum[Identity, A, B, C]

//   type Reader[A] = Base.Sum.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Sum.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Sum.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Sum.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Sum.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Sum.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Sum.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Sum.Writer[Identity, A, B, C]

// final type Union[A] = Base.Union[Identity, Any, ?, A]

// object Union:
//   type Of[A, B] = Base.Union[Identity, Any, A, B]
//   type Via[A, B] = Base.Union[Identity, A, ?, B]
//   type With[A, B, C] = Base.Union[Identity, A, B, C]

//   type Value[A] = Base.Union.Value[Identity, Any, ?, A]

//   object Value:
//     type Of[A, B] = Base.Union.Value[Identity, Any, A, B]
//     type Via[A, B] = Base.Union.Value[Identity, A, ?, B]
//     type With[A, B, C] = Base.Union.Value[Identity, A, B, C]

//     type Required[A] = Base.Union.Value.Required[Identity, Any, ?, A]

//     object Required:
//       type Of[A, B] = Base.Union.Value.Required[Identity, Any, A, B]
//       type Via[A, B] = Base.Union.Value.Required[Identity, A, ?, B]
//       type With[A, B, C] = Base.Union.Value.Required[Identity, A, B, C]

//       type Reader[A] = Base.Union.Value.Required.Reader[Identity, Any, ?, A]

//       object Reader:
//         type Of[A, B] = Base.Union.Value.Required.Reader[Identity, Any, A, B]
//         type Via[A, B] = Base.Union.Value.Required.Reader[Identity, A, ?, B]
//         type With[A, B, C] = Base.Union.Value.Required.Reader[Identity, A, B, C]

//       type Writer[A] = Base.Union.Value.Required.Writer[Identity, Any, ?, A]

//       object Writer:
//         type Of[A, B] = Base.Union.Value.Required.Writer[Identity, Any, A, B]
//         type Via[A, B] = Base.Union.Value.Required.Writer[Identity, A, ?, B]
//         type With[A, B, C] = Base.Union.Value.Required.Writer[Identity, A, B, C]

//     type Reader[A] = Base.Union.Value.Reader[Identity, Any, ?, A]

//     object Reader:
//       type Of[A, B] = Base.Union.Value.Reader[Identity, Any, A, B]
//       type Via[A, B] = Base.Union.Value.Reader[Identity, A, ?, B]
//       type With[A, B, C] = Base.Union.Value.Reader[Identity, A, B, C]

//     type Writer[A] = Base.Union.Value.Writer[Identity, Any, ?, A]

//     object Writer:
//       type Of[A, B] = Base.Union.Value.Writer[Identity, Any, A, B]
//       type Via[A, B] = Base.Union.Value.Writer[Identity, A, ?, B]
//       type With[A, B, C] = Base.Union.Value.Writer[Identity, A, B, C]

//   type Reader[A] = Base.Union.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Union.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Union.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Union.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Union.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Union.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Union.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Union.Writer[Identity, A, B, C]

// final type Branch[A] = Base.Branch[Identity, Any, ?, A]

// object Branch:
//   type Of[A, B] = Base.Branch[Identity, Any, A, B]
//   type Via[A, B] = Base.Branch[Identity, A, ?, B]
//   type With[A, B, C] = Base.Branch[Identity, A, B, C]

//   type Reader[A] = Base.Branch.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Branch.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Branch.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Branch.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Branch.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Branch.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Branch.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Branch.Writer[Identity, A, B, C]

// final type Field[A] = Base.Field[Identity, Any, ?, A]

// object Field:
//   export Base.Field.Null

//   type Of[A, B] = Base.Field[Identity, Any, A, B]
//   type Via[A, B] = Base.Field[Identity, A, ?, B]
//   type With[A, B, C] = Base.Field[Identity, A, B, C]

//   type Reader[A] = Base.Field.Reader[Identity, Any, ?, A]

//   object Reader:
//     type Of[A, B] = Base.Field.Reader[Identity, Any, A, B]
//     type Via[A, B] = Base.Field.Reader[Identity, A, ?, B]
//     type With[A, B, C] = Base.Field.Reader[Identity, A, B, C]

//   type Writer[A] = Base.Field.Writer[Identity, Any, ?, A]

//   object Writer:
//     type Of[A, B] = Base.Field.Writer[Identity, Any, A, B]
//     type Via[A, B] = Base.Field.Writer[Identity, A, ?, B]
//     type With[A, B, C] = Base.Field.Writer[Identity, A, B, C]

object Plain extends Plain
