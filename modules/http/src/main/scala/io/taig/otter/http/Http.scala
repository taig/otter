package io.taig.otter.http

import io.taig.otter as Self
import Self.Codec
import Self.Metadata
import Self.Invariant

object Http:
  sealed abstract class Header[A] extends Product with Serializable

  object Header:
    sealed abstract class Value[A] extends Http.Header[A]

    object Value:
      final case class Constant[A](self: Self.Constant[Http.Header.Value.Primitive, A]) extends Value[A]

      object Constant:
        given codec: Codec.Constant[Http.Header.Value.Constant, Http.Header.Value.Primitive] =
          Codec.Constant(
            lift = [A] => (self: Self.Constant[Http.Header.Value.Primitive, A]) => Constant(self),
            extract = [A] => (codec: Http.Header.Value.Constant[A]) => codec.self
          )

      final case class Enumeration[A](self: Self.Enumeration[Http.Header.Value.Primitive, A]) extends Value[A]

      object Enumeration:
        given codec: Codec.Enumeration[Http.Header.Value.Enumeration, Http.Header.Value.Primitive] =
          Codec.Enumeration(
            lift = [A] => (self: Self.Enumeration[Http.Header.Value.Primitive, A]) => Enumeration(self),
            extract = [A] => (codec: Http.Header.Value.Enumeration[A]) => codec.self
          )

      final case class Primitive[A](self: Self.Primitive.String[A]) extends Value[A]

      object Primitive:
        given codec: Codec.Primitive.String[Http.Header.Value.Primitive] = Codec.Primitive.String(
          lift = [A] => (self: Self.Primitive.String[A]) => Primitive(self),
          extract = [A] => (codec: Http.Header.Value.Primitive[A]) => codec.self
        )

      final case class Union[A](self: Self.Union.Untagged[Http.Header.Value, A]) extends Value[A]

      object Union:
        given codec: Codec.Union.Untagged[Http.Header.Value.Union, Http.Header.Value] =
          Codec.Union.Untagged(
            lift = [A] => (self: Self.Union.Untagged[Http.Header.Value, A]) => Union(self),
            extract = [A] => (codec: Http.Header.Value.Union[A]) => codec.self
          )

      given codec: Codec.Tupleable[Http.Header.Value, Http.Header.Array.Tuple] with
        override def result: Invariant[Array.Tuple] = Http.Header.Array.Tuple.codec
        override def fromElement[A](codec: Http.Header.Value[A]): Http.Header.Value[A] = codec

        extension [A](self: Value[A])
          override def metadata: Metadata = self match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case Primitive(self)   => self.metadata
            case Union(self)       => self.metadata

          override def modifyMetadata(f: Metadata => Metadata): Value[A] = self match
            case Constant(self)    => Constant(self.modifyMetadata(f))
            case Enumeration(self) => Enumeration(self.modifyMetadata(f))
            case Primitive(self)   => Primitive(self.modifyMetadata(f))
            case Union(self)       => Union(self.modifyMetadata(f))

          override def imap[B](f: A => B)(g: B => A): Value[B] = self match
            case Constant(self)    => Constant(self.imap(f)(g))
            case Enumeration(self) => Enumeration(self.imap(f)(g))
            case Primitive(self)   => Primitive(self.imap(f)(g))
            case Union(self)       => Union(self.imap(f)(g))

    sealed abstract class Array[A] extends Http.Header[A]

    object Array:
      final case class Collection[A](self: Self.Collection[Http.Header.Value, A]) extends Http.Header.Array[A]

      object Collection:
        given codec: Codec.Collection[Http.Header.Array.Collection, Http.Header.Value] =
          Codec.Collection(
            lift = [A] => (self: Self.Collection[Http.Header.Value, A]) => Collection(self),
            extract = [A] => (codec: Http.Header.Array.Collection[A]) => codec.self
          )

      final case class Tuple[A](self: Self.Tuple[Http.Header.Value, A]) extends Http.Header.Array[A]

      object Tuple:
        given codec: Codec.Tuple[Http.Header.Array.Tuple, Http.Header.Value] =
          Codec.Tuple(
            lift = [A] => (self: Self.Tuple[Http.Header.Value, A]) => Tuple(self),
            extract = [A] => (codec: Http.Header.Array.Tuple[A]) => codec.self
          )

      given codec: Codec[Http.Header.Array] with
        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def modifyMetadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.modifyMetadata(f))
            case Tuple(self)      => Tuple(self.modifyMetadata(f))

          override def imap[B](f: A => B)(g: B => A): Array[B] = self match
            case Collection(self) => Collection(self.imap(f)(g))
            case Tuple(self)      => Tuple(self.imap(f)(g))

    sealed abstract class Object[A] extends Http.Header[A]

    object Object:
      final case class Dictionary[A](self: Self.Dictionary[Http.Header.Value, Http.Header.Value, A])
          extends Http.Header.Object[A]

      object Dictionary:
        given codec: Codec.Dictionary[Http.Header.Object.Dictionary, Http.Header.Value, Http.Header.Value] =
          Codec.Dictionary(
            lift = [A] => (self: Self.Dictionary[Http.Header.Value, Http.Header.Value, A]) => Dictionary(self),
            extract = [A] => (codec: Http.Header.Object.Dictionary[A]) => codec.self
          )

      final case class Record[A](self: Self.Record[Http.Header.Value, Http.Header.Value, A])
          extends Http.Header.Object[A]

      object Record:
        given codec: Codec.Record[Http.Header.Object.Record, Http.Header.Value, Http.Header.Value] =
          Codec.Record(
            lift = [A] => (self: Self.Record[Http.Header.Value, Http.Header.Value, A]) => Record(self),
            extract = [A] => (codec: Http.Header.Object.Record[A]) => codec.self
          )

      given codec: Codec[Http.Header.Object] with
        extension [A](self: Object[A])
          override def metadata: Metadata = self match
            case Dictionary(self) => self.metadata
            case Record(self)     => self.metadata

          override def modifyMetadata(f: Metadata => Metadata): Object[A] = self match
            case Dictionary(self) => Dictionary(self.modifyMetadata(f))
            case Record(self)     => Record(self.modifyMetadata(f))

          override def imap[B](f: A => B)(g: B => A): Object[B] = self match
            case Dictionary(self) => Dictionary(self.imap(f)(g))
            case Record(self)     => Record(self.imap(f)(g))

  sealed abstract class Segment[A] extends Product with Serializable

  object Segment:
    sealed abstract class Value[A] extends Http.Segment[A]

    object Value:
      final case class Constant[A](self: Self.Constant[Http.Segment.Value.Primitive, A]) extends Value[A]

      object Constant:
        given codec: Codec.Constant[Http.Segment.Value.Constant, Http.Segment.Value.Primitive] =
          Codec.Constant(
            lift = [A] => (self: Self.Constant[Http.Segment.Value.Primitive, A]) => Constant(self),
            extract = [A] => (codec: Http.Segment.Value.Constant[A]) => codec.self
          )

      final case class Enumeration[A](self: Self.Enumeration[Http.Segment.Value.Primitive, A]) extends Value[A]

      object Enumeration:
        given codec: Codec.Enumeration[Http.Segment.Value.Enumeration, Http.Segment.Value.Primitive] =
          Codec.Enumeration(
            lift = [A] => (self: Self.Enumeration[Http.Segment.Value.Primitive, A]) => Enumeration(self),
            extract = [A] => (codec: Http.Segment.Value.Enumeration[A]) => codec.self
          )

      final case class Primitive[A](self: Self.Primitive.String[A]) extends Value[A]

      object Primitive:
        given codec: Codec.Primitive.String[Http.Segment.Value.Primitive] = Codec.Primitive.String(
          lift = [A] => (self: Self.Primitive.String[A]) => Primitive(self),
          extract = [A] => (codec: Http.Segment.Value.Primitive[A]) => codec.self
        )

      final case class Union[A](self: Self.Union.Untagged[Http.Segment.Value, A]) extends Value[A]

      object Union:
        given codec: Codec.Union.Untagged[Http.Segment.Value.Union, Http.Segment.Value] =
          Codec.Union.Untagged(
            lift = [A] => (self: Self.Union.Untagged[Http.Segment.Value, A]) => Union(self),
            extract = [A] => (codec: Http.Segment.Value.Union[A]) => codec.self
          )

      given codec: Codec.Tupleable[Http.Segment.Value, Http.Segment.Array.Tuple] with
        override def result: Invariant[Array.Tuple] = Http.Segment.Array.Tuple.codec
        override def fromElement[A](codec: Http.Segment.Value[A]): Http.Segment.Value[A] = codec

        extension [A](self: Value[A])
          override def metadata: Metadata = self match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case Primitive(self)   => self.metadata
            case Union(self)       => self.metadata

          override def modifyMetadata(f: Metadata => Metadata): Value[A] = self match
            case Constant(self)    => Constant(self.modifyMetadata(f))
            case Enumeration(self) => Enumeration(self.modifyMetadata(f))
            case Primitive(self)   => Primitive(self.modifyMetadata(f))
            case Union(self)       => Union(self.modifyMetadata(f))

          override def imap[B](f: A => B)(g: B => A): Value[B] = self match
            case Constant(self)    => Constant(self.imap(f)(g))
            case Enumeration(self) => Enumeration(self.imap(f)(g))
            case Primitive(self)   => Primitive(self.imap(f)(g))
            case Union(self)       => Union(self.imap(f)(g))

    sealed abstract class Array[A] extends Http.Segment[A]

    object Array:
      final case class Collection[A](self: Self.Collection[Http.Segment.Value, A]) extends Http.Segment.Array[A]

      object Collection:
        given codec: Codec.Collection[Http.Segment.Array.Collection, Http.Segment.Value] =
          Codec.Collection(
            lift = [A] => (self: Self.Collection[Http.Segment.Value, A]) => Collection(self),
            extract = [A] => (codec: Http.Segment.Array.Collection[A]) => codec.self
          )

      final case class Tuple[A](self: Self.Tuple[Http.Segment.Value, A]) extends Http.Segment.Array[A]

      object Tuple:
        given codec: Codec.Tuple[Http.Segment.Array.Tuple, Http.Segment.Value] =
          Codec.Tuple(
            lift = [A] => (self: Self.Tuple[Http.Segment.Value, A]) => Tuple(self),
            extract = [A] => (codec: Http.Segment.Array.Tuple[A]) => codec.self
          )

      given codec: Codec[Http.Segment.Array] with
        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def modifyMetadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.modifyMetadata(f))
            case Tuple(self)      => Tuple(self.modifyMetadata(f))

          override def imap[B](f: A => B)(g: B => A): Array[B] = self match
            case Collection(self) => Collection(self.imap(f)(g))
            case Tuple(self)      => Tuple(self.imap(f)(g))

    sealed abstract class Object[A] extends Http.Segment[A]

    object Object:
      final case class Dictionary[A](self: Self.Dictionary[Http.Segment.Value, Http.Segment.Value, A])
          extends Http.Segment.Object[A]

      object Dictionary:
        given codec: Codec.Dictionary[Http.Segment.Object.Dictionary, Http.Segment.Value, Http.Segment.Value] =
          Codec.Dictionary(
            lift = [A] => (self: Self.Dictionary[Http.Segment.Value, Http.Segment.Value, A]) => Dictionary(self),
            extract = [A] => (codec: Http.Segment.Object.Dictionary[A]) => codec.self
          )

      final case class Record[A](self: Self.Record[Http.Segment.Value, Http.Segment.Value, A])
          extends Http.Segment.Object[A]

      object Record:
        given codec: Codec.Record[Http.Segment.Object.Record, Http.Segment.Value, Http.Segment.Value] =
          Codec.Record(
            lift = [A] => (self: Self.Record[Http.Segment.Value, Http.Segment.Value, A]) => Record(self),
            extract = [A] => (codec: Http.Segment.Object.Record[A]) => codec.self
          )

      given codec: Codec[Http.Segment.Object] with
        extension [A](self: Object[A])
          override def metadata: Metadata = self match
            case Dictionary(self) => self.metadata
            case Record(self)     => self.metadata

          override def modifyMetadata(f: Metadata => Metadata): Object[A] = self match
            case Dictionary(self) => Dictionary(self.modifyMetadata(f))
            case Record(self)     => Record(self.modifyMetadata(f))

          override def imap[B](f: A => B)(g: B => A): Object[B] = self match
            case Dictionary(self) => Dictionary(self.imap(f)(g))
            case Record(self)     => Record(self.imap(f)(g))
