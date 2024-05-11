package io.taig.otter

import io.taig.otter as Base

trait Types[F[+_]]:
  // final type Schema[A] = Schema.Of[F[Base.Schema[*, ?]], A]

  // object Schema:
  //   type Of[+Of, A] = F[[_] =>> Base.Schema[Of, A]]

  //   type Reader[+A] = Reader.Of[F[Base.Schema.Reader[*, ?]], A]

  //   object Reader:
  //     type Of[+Of, +A] = F[[_] =>> Base.Schema.Reader[Of, A]]

  //   type Writer[-A] = Writer.Of[F[Base.Schema.Writer[*, ?]], A]

  //   object Writer:
  //     type Of[+Of, -A] = F[[_] =>> Base.Schema.Writer[Of, A]]

  // final type Collection[A] = Collection.Of[F[Base.Schema[*, ?]], A]

  // object Collection:
  //   type Of[+Of, A] = F[[_] =>> Base.Collection[Of, A]]

  //   type Reader[+A] = Reader.Of[F[Base.Schema.Reader[*, ?]], A]

  //   object Reader:
  //     type Of[+Of, +A] = F[[_] =>> Base.Collection.Reader[Of, A]]

  //   type Writer[-A] = Writer.Of[F[Base.Schema.Writer[*, ?]], A]

  //   object Writer:
  //     type Of[+Of, -A] = F[[_] =>> Base.Collection.Writer[Of, A]]

  final type Primitive[A] = F[Base.Primitive[A]]

  object Primitive:
    // type Required[A] = F[[_] =>> Base.Primitive.Required[A]]

    // object Required:
    //   type Reader[+A] = F[[_] =>> Base.Primitive.Required.Reader[A]]
    //   type Writer[-A] = F[[_] =>> Base.Primitive.Required.Writer[A]]

    type Reader[+A] = F[Base.Primitive.Reader[A]]
    type Writer[-A] = F[Base.Primitive.Writer[A]]
