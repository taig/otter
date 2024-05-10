package io.taig.otter

import io.taig.otter as Base

abstract class Dsl[F[+_[+_]]]:
  final type Schema[A] = Schema.Of[F[Base.Schema[*, ?]], A]

  object Schema:
    type Of[+Of, A] = F[[_] =>> Base.Schema[Of, A]]

    type Reader[+A] = Reader.Of[F[Base.Schema.Reader[*, ?]], A]

    object Reader:
      type Of[+Of, +A] = F[[_] =>> Base.Schema.Reader[Of, A]]

    type Writer[-A] = Writer.Of[F[Base.Schema.Writer[*, ?]], A]

    object Writer:
      type Of[+Of, -A] = F[[_] =>> Base.Schema.Writer[Of, A]]

  final type Collection[A] = Collection.Of[F[Base.Schema[*, ?]], A]

  object Collection:
    type Of[+Of, A] = F[[_] =>> Base.Schema[Of, A]]

    type Reader[+A] = Reader.Of[F[Base.Schema.Reader[*, ?]], A]

    object Reader:
      type Of[+Of, +A] = F[[_] =>> Base.Collection.Reader[Of, A]]

    type Writer[-A] = Writer.Of[F[Base.Schema.Writer[*, ?]], A]

    object Writer:
      type Of[+Of, -A] = F[[_] =>> Base.Collection.Writer[Of, A]]

//   // trait Schemas:
//   //   type Of[+Of, A] <: Schema.Reader.Of[Of, A] & Schema.Writer.Of[Of, A]

//   //   final type Reader[A] = Reader.Of[ReaderLub, A]

//   //   trait Readers:
//   //     type Of[+Of, A]

//   //   val Reader: Readers

//   //   final type Writer[A] = Writer.Of[WriterLub, A]

//   //   trait Writers:
//   //     type Of[+Of, A]

//   //   val Writer: Writers

//   // val Schema: Schemas

//   final type Collection[A] = Collection.Of[Lub, A]

//   trait Collections:
//     type Of[+Of, A] <: Schema.Of[Of, A] & Collection.Reader.Of[Of, A] & Collection.Writer.Of[Of, A]

//     final type Reader[A] = Collection.Reader.Of[ReaderLub, A]

//     trait Readers:
//       type Of[+Of, A] <: Schema.Reader.Of[Of, A]

//     val Reader: Readers

//     final type Writer[A] = Collection.Writer.Of[WriterLub, A]

//     trait Writers:
//       type Of[+Of, A] <: Schema.Writer.Of[Of, A]

//     val Writer: Writers

//   val Collection: Collections

//   type Primitive[A] <: Schema[A] & Primitive.Reader[A] & Primitive.Writer[A]

//   trait Primitives:
//     type Reader[A] <: Schema.Reader[A]

//     type Writer[A] <: Schema.Writer[A]

//     type Required[A] <: Primitive[A] & Primitive.Required.Reader[A] & Primitive.Required.Writer[A]

//     trait Requireds:
//       type Reader[A] <: Primitive.Reader[A]

//       type Writer[A] <: Primitive.Writer[A]

//     val Required: Requireds

//   val Primitive: Primitives

//   final type Tuple[A] = Tuple.Of[Lub, A]

//   trait Tuples:
//     type Of[+Of, A] <: Schema.Of[Of, A] & Tuple.Reader.Of[Of, A] & Tuple.Writer.Of[Of, A]

//     final type Reader[A] = Reader.Of[ReaderLub, A]

//     trait Readers:
//       type Of[+Of, A] <: Schema.Reader.Of[Of, A]

//     val Reader: Readers

//     final type Writer[A] = Writer.Of[WriterLub, A]

//     trait Writers:
//       type Of[+Of, A] <: Schema.Writer.Of[Of, A]

//     val Writer: Writers

//   val Tuple: Tuples
