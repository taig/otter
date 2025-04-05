package io.taig.otter.http
import io.taig.otter.*
import org.typelevel.ci.CIString
import cats.data.Chain

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
      delimiter: Delimiter,
      metadata: Metadata
  ) extends Header[A]

  final private[otter] case class Object[A](
      name: CIString,
      codec: Reference[Header.Codec.Object, A],
      metadata: Metadata
  ) extends Header[A]

  final private[otter] case class Optional[A](self: Header[A]) extends Header[Option[A]]:
    export self.{codec, metadata, name}

  final private[otter] case class Modify[A, B](self: Header[A], f: A => B, g: B => A) extends Header[B]:
    export self.{codec, metadata, name}

  final case class Codec[A](
      value: Constant[Header.Codec, A] | Enumeration[Header.Codec, A] | Primitive.String[A] |
        Union.Untagged[Header.Codec, A]
  ) extends AnyVal

  object Codec:
    final case class Array[A](value: Collection[Header.Codec, A] | Tuple[Header.Codec, A]) extends AnyVal

    final case class Object[A](value: Dictionary[Header.Codec, Header.Codec, A] | Record[Header.Codec, Header.Codec, A])
        extends AnyVal
