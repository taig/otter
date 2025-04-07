package io.taig.otter.http

import io.taig.otter as Self
import Self.Reference
import Self.CodecInvariant
import Self.PrimitiveInvariant
import Self.Metadata

sealed abstract class Segment[A] extends Product, Serializable:
  def name: String

  final def toPath: Path[A] = Path.Root(segment = this)

object Segment:
  final case class Static(name: String) extends Segment[Unit]

  sealed abstract class Parameter[A] extends Segment[A]

  object Parameter:
    final private[otter] case class Array[A](
        name: String,
        codec: Reference[Segment.Codec.Array, A],
        explode: Boolean,
        style: Segment.Style
    ) extends Segment.Parameter[A]

    final private[otter] case class Modify[A, B](self: Segment.Parameter[A], f: A => B, g: B => A)
        extends Segment.Parameter[B]:
      export self.name
    final private[otter] case class Object[A](
        name: String,
        codec: Reference[Segment.Codec.Object, A],
        explode: Boolean,
        style: Segment.Style
    ) extends Segment.Parameter[A]
    final private[otter] case class Value[A](name: String, codec: Reference[Segment.Codec, A], style: Segment.Style)
        extends Segment.Parameter[A]

  enum Style:
    case Label
    case Matrix
    case Simple

  sealed abstract class Codec[A] extends Product with Serializable

  object Codec:
    final case class Constant[A](self: Self.Constant[Segment.Codec.Primitive, A]) extends Codec[A]

    final case class Enumeration[A](self: Self.Enumeration[Segment.Codec.Primitive, A]) extends Codec[A]

    final case class Primitive[A](self: Self.Primitive[A]) extends Codec[A]

    object Primitive:
      given invariant: PrimitiveInvariant.String[Segment.Codec.Primitive] =
        new PrimitiveInvariant.String.Lift[Segment.Codec.Primitive]:
          override def lift[A](codec: Self.Primitive.String[A]): Segment.Codec.Primitive[A] = Primitive(self = codec)
          extension [A](self: Primitive[A])
            override def imap[B](f: A => B)(g: B => A): Segment.Codec.Primitive[B] =
              Primitive(self = self.self.imap(f)(g))
            override def metadata: Metadata = self.self.metadata
            override def modifyMetadata(f: Metadata => Metadata): Segment.Codec.Primitive[A] =
              Primitive(self = self.self.modifyMetadata(f))

    final case class Union[A](self: Self.Union.Untagged[Segment.Codec, A]) extends Codec[A]

    sealed abstract class Array[A] extends Product with Serializable

    object Array:
      final case class Collection[A](self: Self.Collection[Segment.Codec, A]) extends Segment.Codec.Array[A]

      final case class Tuple[A](self: Self.Tuple[Segment.Codec, A]) extends Segment.Codec.Array[A]

    sealed abstract class Object[A] extends Product with Serializable

    object Object:
      final case class Dictionary[A](self: Self.Dictionary[Segment.Codec, Segment.Codec, A])
          extends Segment.Codec.Object[A]

      final case class Record[A](self: Self.Record[Segment.Codec, Segment.Codec, A]) extends Segment.Codec.Object[A]
