package io.taig.otter.http.schema

import io.taig.otter.http.HttpExport.*
import io.taig.otter.schema.Schema
import io.taig.otter.http.HttpExport.*

trait ResponseSchema[Self[+_[_], _]] extends SchemaK[Self]:
  self =>
  extension [S[_], A](self: Self[S, A]) def modifyResults[B](f: Results[S, A] => Results[S, B]): Self[S, B]

  override def imapK[T[+_[_], _]](fK: [S[_], A] => Self[S, A] => T[S, A])(
      gK: [S[_], A] => T[S, A] => Self[S, A]
  ): ResponseSchema[T] =
    new ResponseSchema[T]:
      extension [S[_], A](ta: T[S, A])
        override def modifyResults[B](f: Results[S, A] => Results[S, B]): T[S, B] =
          ???

      override def algebra[S[_]]: Schema[T[S, *]] = self
        .algebra[S]
        .imapK(
          [A] => (self: Self[S, A]) => fK(self)
        )([A] => (value: T[S, A]) => gK(value))
