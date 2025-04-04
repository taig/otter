package io.taig.otter.http
import io.taig.otter.*
import org.typelevel.ci.CIString
import cats.data.Chain

sealed abstract class Header[A] extends Product, Serializable:
  def name: CIString

  def codec: Reference[Header.Codec, ?]

  def metadata: Metadata

object Header:
  final case class Value[A](name: CIString, codec: Reference[Header.Codec.Value, A], metadata: Metadata)
      extends Header[A]

  final case class Array[A](self: Header[A], delimiter: Delimiter) extends Header[Chain[A]]:
    export self.{codec, metadata, name}

  final private[otter] case class Optional[A](self: Header[A]) extends Header[Option[A]]:
    export self.{codec, metadata, name}

  final private[otter] case class Modify[A, B](self: Header[A], f: A => B, g: B => A) extends Header[B]:
    export self.{codec, metadata, name}

  sealed abstract class Codec[A]:
    def value: Enumeration[Header.Codec.Value, A] | Primitive[A] | Union.Untagged[Header.Codec.Value, A] |
      Collection[Header.Codec.Value, A] | Tuple[Header.Codec.Value, A] | Record[Header.Codec.Value, A]

  object Codec:
    final case class Value[A](
        value: Enumeration[Header.Codec.Value, A] | Primitive[A] | Union.Untagged[Header.Codec.Value, A]
    ) extends Header.Codec[A]

    final case class Array[A](value: Collection[Header.Codec.Value, A] | Tuple[Header.Codec.Value, A])
        extends Header.Codec[A]

    final case class Object[A](value: Record[Header.Codec.Value, A]) extends Header.Codec[A]
