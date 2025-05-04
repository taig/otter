package io.taig.otter.http

import io.taig.otter.+
import io.taig.otter.http.header.MediaType

abstract class BodyEncoder[S[_]]:
  def apply[A](mediaType: MediaType, codec: S[A], a: A): Array[Byte]

  final def or[T[_]](encoder: BodyEncoder[T]): BodyEncoder[S + T] = new BodyEncoder[S + T]:
    override def apply[A](mediaType: MediaType, codec: (S + T)[A], a: A): Array[Byte] = ???

abstract class BodyEncoder2[S[_]]:
  def apply[A](mediaType: MediaType, codec: Body[S, A], a: A): Array[Byte] = codec match
    case Body.Empty                  => Array.emptyByteArray
    case Body.Modify(self, _, g)     => apply(mediaType, codec = self, g(a))
    case Body.Or(left, right)        => ???
    case Body.OrElse(left, right)    => ???
    case Body.Root(mediaType, codec) => ???
