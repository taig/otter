package io.taig.otter.http

import io.taig.otter.+
import io.taig.otter.http.header.MediaType

abstract class BodyEncoder[-S[_]]:
  def apply[A](mediaType: MediaType, codec: S[A], a: A): Array[Byte]

  final def or[T[_]](encoder: BodyEncoder[T]): BodyEncoder[S + T] = new BodyEncoder[S + T]:
    override def apply[A](mediaType: MediaType, codec: (S + T)[A], a: A): Array[Byte] = ???
