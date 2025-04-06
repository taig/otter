package io.taig.otter.http

import io.taig.otter.*
import io.taig.otter as Self
import org.typelevel.ci.CIString

sealed abstract class Header[A] extends Product, Serializable:
  def name: CIString

  def codec: Reference[[a] =>> Header.Codec[a] | Header.Codec.Array[a] | Header.Codec.Object[a], ?]

  def metadata: Metadata

object Header:
  final private[otter] case class Value[A](name: CIString, codec: Reference[Header.Codec, A], metadata: Metadata)
      extends Header[A]

  final private[otter] case class Array[A](
      name: CIString,
      codec: Reference[Header.Codec.Array, A],
      metadata: Metadata
  ) extends Header[A]

  final private[otter] case class Object[A](
      name: CIString,
      codec: Reference[Header.Codec.Object, A],
      explode: Boolean,
      metadata: Metadata
  ) extends Header[A]

  final private[otter] case class Optional[A](self: Header[A]) extends Header[Option[A]]:
    export self.{codec, metadata, name}

  final private[otter] case class Modify[A, B](self: Header[A], f: A => B, g: B => A) extends Header[B]:
    export self.{codec, metadata, name}

  sealed abstract class Codec[A] extends Product, Serializable

  object Codec:
    final case class Constant[A](self: Self.Constant[Header.Codec.Primitive, A]) extends Header.Codec[A]

    final case class Enumeration[A](self: Self.Enumeration[Header.Codec.Primitive, A]) extends Header.Codec[A]

    final case class Primitive[A](self: Self.Primitive.String[A]) extends Header.Codec[A]

    final case class Union[A](self: Self.Union.Untagged[Header.Codec, A]) extends Header.Codec[A]

    sealed abstract class Array[A] extends Product with Serializable

    object Array:
      final case class Collection[A](self: Self.Collection[Header.Codec, A]) extends Header.Codec.Array[A]

      final case class Tuple[A](self: Self.Tuple[Header.Codec, A]) extends Header.Codec.Array[A]

    sealed abstract class Object[A] extends Product with Serializable

    object Object:
      final case class Dictionary[A](self: Self.Dictionary[Header.Codec, Header.Codec, A])
          extends Header.Codec.Object[A]

      final case class Record[A](self: Self.Record[Header.Codec, Header.Codec, A]) extends Header.Codec.Object[A]
