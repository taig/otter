package io.taig.otter.http.schema

import io.taig.otter.schema.Schema

trait SchemaK[Self[+_[_], _]]:
  def schema[S[_]]: Schema[Self[S, *]]

  extension [S[_], A](self: Self[S, A])
    def imap[B](f: A => B)(g: B => A): Self[S, B] = schema[S].imap(self)(f)(g)