// package io.taig.otter

// import io.taig.otter as Plain

// trait Types:
//   val Metadata: Metadatas

//   final type Schema[A] = Plain.Schema[Metadata.Schema, A]
//   final type Value[A] = Plain.Value[Metadata.Value, A]

//   final type Primitive[A] = Plain.Primitive[Metadata.Primitive, A]

//   object Primitive:
//     final type Required[A] = Plain.Primitive.Required[Metadata.Primitive, A]

//   type Tuple[A] = Plain.Tuple[Metadata.Tuple, A]

//   object Tuple:
//     type Of[S <: Schema[?], A] = Plain.Tuple.Of[S, Metadata.Tuple, A]
