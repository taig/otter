package io.taig.otter.http

import cats.data.Validated
import cats.parse.Parser
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Codec
import io.taig.otter.Invariant
import io.taig.otter.Metadata
import io.taig.otter.Violation
import io.taig.otter.Violations

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

      given codec: Codec.Extension.Tupleable[Http.Header.Value, Http.Header.Array.Tuple] with
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

  sealed abstract class Query[A] extends Product with Serializable

  object Query:
    sealed abstract class Value[A] extends Http.Query[A]

    object Value:
      final case class Constant[A](self: Self.Constant[Http.Query.Value.Primitive, A]) extends Value[A]

      object Constant:
        given codec: Codec.Constant[Http.Query.Value.Constant, Http.Query.Value.Primitive] =
          Codec.Constant(
            lift = [A] => (self: Self.Constant[Http.Query.Value.Primitive, A]) => Constant(self),
            extract = [A] => (codec: Http.Query.Value.Constant[A]) => codec.self
          )

      final case class Enumeration[A](self: Self.Enumeration[Http.Query.Value.Primitive, A]) extends Value[A]

      object Enumeration:
        given codec: Codec.Enumeration[Http.Query.Value.Enumeration, Http.Query.Value.Primitive] =
          Codec.Enumeration(
            lift = [A] => (self: Self.Enumeration[Http.Query.Value.Primitive, A]) => Enumeration(self),
            extract = [A] => (codec: Http.Query.Value.Enumeration[A]) => codec.self
          )

      final case class Primitive[A](self: Self.Primitive.String[A]) extends Value[A]

      object Primitive:
        given codec: Codec.Primitive.String[Http.Query.Value.Primitive] = Codec.Primitive.String(
          lift = [A] => (self: Self.Primitive.String[A]) => Primitive(self),
          extract = [A] => (codec: Http.Query.Value.Primitive[A]) => codec.self
        )

      final case class Union[A](self: Self.Union.Untagged[Http.Query.Value, A]) extends Value[A]

      object Union:
        given codec: Codec.Union.Untagged[Http.Query.Value.Union, Http.Query.Value] =
          Codec.Union.Untagged(
            lift = [A] => (self: Self.Union.Untagged[Http.Query.Value, A]) => Union(self),
            extract = [A] => (codec: Http.Query.Value.Union[A]) => codec.self
          )

      given codec: Codec.Extension.Tupleable[Http.Query.Value, Http.Query.Array.Tuple] with
        override def result: Invariant[Array.Tuple] = Http.Query.Array.Tuple.codec
        override def fromElement[A](codec: Http.Query.Value[A]): Http.Query.Value[A] = codec

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

    sealed abstract class Array[A] extends Http.Query[A]

    object Array:
      final case class Collection[A](self: Self.Collection[Http.Query.Value, A]) extends Http.Query.Array[A]

      object Collection:
        given codec: Codec.Collection[Http.Query.Array.Collection, Http.Query.Value] =
          Codec.Collection(
            lift = [A] => (self: Self.Collection[Http.Query.Value, A]) => Collection(self),
            extract = [A] => (codec: Http.Query.Array.Collection[A]) => codec.self
          )

      final case class Tuple[A](self: Self.Tuple[Http.Query.Value, A]) extends Http.Query.Array[A]

      object Tuple:
        given codec: Codec.Tuple[Http.Query.Array.Tuple, Http.Query.Value] =
          Codec.Tuple(
            lift = [A] => (self: Self.Tuple[Http.Query.Value, A]) => Tuple(self),
            extract = [A] => (codec: Http.Query.Array.Tuple[A]) => codec.self
          )

      given codec: Codec[Http.Query.Array] with
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

    sealed abstract class Object[A] extends Http.Query[A]

    object Object:
      final case class Dictionary[A](self: Self.Dictionary[Http.Query.Value, Http.Query.Value, A])
          extends Http.Query.Object[A]

      object Dictionary:
        given codec: Codec.Dictionary[Http.Query.Object.Dictionary, Http.Query.Value, Http.Query.Value] =
          Codec.Dictionary(
            lift = [A] => (self: Self.Dictionary[Http.Query.Value, Http.Query.Value, A]) => Dictionary(self),
            extract = [A] => (codec: Http.Query.Object.Dictionary[A]) => codec.self
          )

      final case class Record[A](self: Self.Record[Http.Query.Value, Http.Query.Value, A]) extends Http.Query.Object[A]

      object Record:
        given codec: Codec.Record[Http.Query.Object.Record, Http.Query.Value, Http.Query.Value] =
          Codec.Record(
            lift = [A] => (self: Self.Record[Http.Query.Value, Http.Query.Value, A]) => Record(self),
            extract = [A] => (codec: Http.Query.Object.Record[A]) => codec.self
          )

      given codec: Codec[Http.Query.Object] with
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

    final case class Optional[A](self: Self.Nullable[Http.Query, A]) extends Http.Query[A]

    object Optional:
      given codec: Codec.Nullable[Http.Query.Optional, Http.Query] = Codec.Nullable(
        lift = [A] => (self: Self.Nullable[Http.Query, A]) => Optional(self),
        extract = [A] => (codec: Http.Query.Optional[A]) => codec.self
      )

  sealed abstract class Parameter[A] extends Product with Serializable

  object Parameter:
    sealed abstract class Value[A] extends Http.Parameter[A]

    object Value:
      final case class Constant[A](self: Self.Constant[Http.Parameter.Value.Primitive, A]) extends Value[A]

      object Constant:
        given codec: Codec.Constant[Http.Parameter.Value.Constant, Http.Parameter.Value.Primitive] =
          Codec.Constant(
            lift = [A] => (self: Self.Constant[Http.Parameter.Value.Primitive, A]) => Constant(self),
            extract = [A] => (codec: Http.Parameter.Value.Constant[A]) => codec.self
          )

      final case class Enumeration[A](self: Self.Enumeration[Http.Parameter.Value.Primitive, A]) extends Value[A]

      object Enumeration:
        given codec: Codec.Enumeration[Http.Parameter.Value.Enumeration, Http.Parameter.Value.Primitive] =
          Codec.Enumeration(
            lift = [A] => (self: Self.Enumeration[Http.Parameter.Value.Primitive, A]) => Enumeration(self),
            extract = [A] => (codec: Http.Parameter.Value.Enumeration[A]) => codec.self
          )

      final case class Primitive[A](self: Self.Primitive.String[A]) extends Value[A]

      object Primitive:
        given codec: Codec.Primitive.String[Http.Parameter.Value.Primitive] = Codec.Primitive.String(
          lift = [A] => (self: Self.Primitive.String[A]) => Primitive(self),
          extract = [A] => (codec: Http.Parameter.Value.Primitive[A]) => codec.self
        )

      final case class Union[A](self: Self.Union.Untagged[Http.Parameter.Value, A]) extends Value[A]

      object Union:
        given codec: Codec.Union.Untagged[Http.Parameter.Value.Union, Http.Parameter.Value] =
          Codec.Union.Untagged(
            lift = [A] => (self: Self.Union.Untagged[Http.Parameter.Value, A]) => Union(self),
            extract = [A] => (codec: Http.Parameter.Value.Union[A]) => codec.self
          )

      given codec: Codec.Extension.Tupleable[Http.Parameter.Value, Http.Parameter.Array.Tuple] with
        override def result: Invariant[Array.Tuple] = Http.Parameter.Array.Tuple.codec
        override def fromElement[A](codec: Http.Parameter.Value[A]): Http.Parameter.Value[A] = codec

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

    sealed abstract class Array[A] extends Http.Parameter[A]

    object Array:
      final case class Collection[A](self: Self.Collection[Http.Parameter.Value, A]) extends Http.Parameter.Array[A]

      object Collection:
        given codec: Codec.Collection[Http.Parameter.Array.Collection, Http.Parameter.Value] =
          Codec.Collection(
            lift = [A] => (self: Self.Collection[Http.Parameter.Value, A]) => Collection(self),
            extract = [A] => (codec: Http.Parameter.Array.Collection[A]) => codec.self
          )

      final case class Tuple[A](self: Self.Tuple[Http.Parameter.Value, A]) extends Http.Parameter.Array[A]

      object Tuple:
        given codec: Codec.Tuple[Http.Parameter.Array.Tuple, Http.Parameter.Value] =
          Codec.Tuple(
            lift = [A] => (self: Self.Tuple[Http.Parameter.Value, A]) => Tuple(self),
            extract = [A] => (codec: Http.Parameter.Array.Tuple[A]) => codec.self
          )

      given codec: Codec[Http.Parameter.Array] with
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

    sealed abstract class Object[A] extends Http.Parameter[A]

    object Object:
      final case class Dictionary[A](self: Self.Dictionary[Http.Parameter.Value, Http.Parameter.Value, A])
          extends Http.Parameter.Object[A]

      object Dictionary:
        given codec: Codec.Dictionary[Http.Parameter.Object.Dictionary, Http.Parameter.Value, Http.Parameter.Value] =
          Codec.Dictionary(
            lift = [A] => (self: Self.Dictionary[Http.Parameter.Value, Http.Parameter.Value, A]) => Dictionary(self),
            extract = [A] => (codec: Http.Parameter.Object.Dictionary[A]) => codec.self
          )

      final case class Record[A](self: Self.Record[Http.Parameter.Value, Http.Parameter.Value, A])
          extends Http.Parameter.Object[A]

      object Record:
        given codec: Codec.Record[Http.Parameter.Object.Record, Http.Parameter.Value, Http.Parameter.Value] =
          Codec.Record(
            lift = [A] => (self: Self.Record[Http.Parameter.Value, Http.Parameter.Value, A]) => Record(self),
            extract = [A] => (codec: Http.Parameter.Object.Record[A]) => codec.self
          )

      given codec: Codec[Http.Parameter.Object] with
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

extension [A](self: Either[Parser.Error, A])
  private[otter] def toValidatedViolations(tpe: String, value: String): Validated[Violations, A] =
    self.toValidated.leftMap: error =>
      Violations.rootNec(Violation.tpe(name = tpe, actual = value, hint = error.show))
