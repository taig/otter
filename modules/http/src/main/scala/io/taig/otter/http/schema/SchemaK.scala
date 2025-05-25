package io.taig.otter.http.schema

import io.taig.otter.schema.Schema

trait SchemaK[Self[+_[_], _]]:
  self =>

  def algebra[S[_]]: Schema[Self[S, *]]

  def imapK[T[+_[_], _]](fK: [S[_], A] => Self[S, A] => T[S, A])(
      gK: [S[_], A] => T[S, A] => Self[S, A]
  ): SchemaK[T] = new SchemaK[T]:
    override def algebra[S[_]]: Schema[T[S, *]] = self
      .algebra[S]
      .imapK(
        [A] => (self: Self[S, A]) => fK(self)
      )([A] => (value: T[S, A]) => gK(value))
