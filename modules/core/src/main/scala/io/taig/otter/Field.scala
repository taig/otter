package io.taig.otter

import io.taig.otter.Codec.Result

sealed abstract class Field[+O <: Data, A]:
  self =>

  def name: String

  def codec: Codec[?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Field[O, A] = new Field[O, A]:
    export self.{codec, decode, encode, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Field[O, B] = new Field[O, B]:
    export self.{codec, metadata, name}
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  def decode(data: Data): Codec.Result[A]

  def encode(a: A): O

object Field:
  def apply[O <: Data, A](name: String, codec: Codec[O, A]): Field[O, A] =
    val _name = name
    val _codec = codec

    new Field[O, A]:
      override def name: String = _name
      override def codec: Codec[O, A] = _codec
      override def metadata: Metadata = Metadata.Empty
      override def decode(data: Data): Codec.Result[A] = ???
      override def encode(a: A): O = codec.encode(a)
