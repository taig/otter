package io.taig.otter

import cats.syntax.all.*

sealed abstract class Field[+O <: Data, A]:
  self =>

  def name: String

  def codec: Codec[?, ?, ?]

  final def nulls: Attribute.Optional[Field[O, A], Null] = Attribute.Optional(this, Keys.nulls)

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Field[O, A] = new Field[O, A]:
    export self.{codec, decode, encode, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Field[O, B] = new Field[O, B]:
    export self.{codec, metadata, name}
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  final def :*[P <: Data, B](field: Field[P, B])(using merge: Evidence.Merge[A, B]): Fields[O | P, merge.Out] =
    toFields :* field

  final def *:[P <: Data, B](field: Field[P, B])(using merge: Evidence.Merge[B, A]): Fields[P | O, merge.Out] =
    field *: toFields

  final def toFields: Fields[O, A] = Fields(this)

  def decode(data: Data): Codec.Result[A]

  final def encode(a: A, parent: Null): Option[(String, O)] =
    val hideNull = (parent, nulls.value) match
      case (_, Some(nulls)) => nulls === Null.Hide
      case (nulls, None)    => nulls === Null.Hide

    encode(a) match
      case Data.Null if hideNull => None
      case data                  => Some((name, data))

  def encode(a: A): O

object Field:
  def apply[F[+a] <: Data.Optional[a], O <: Data, A](name: String, of: => Codec[F, O, A]): Field[F[O], A] =
    val _name = name

    new Field[F[O], A]:
      override def name: String = _name
      override def codec: Codec[?, ?, ?] = of
      override def metadata: Metadata = Metadata.Empty
      override def decode(data: Data): Codec.Result[A] = of.decode(data)
      override def encode(a: A): F[O] = of.encode(a)

  given [O <: Data, A]: Metadata.Ops[Field[O, A]] with
    extension (self: Field[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Field[O, A] = self.modifyMetadata(f)
