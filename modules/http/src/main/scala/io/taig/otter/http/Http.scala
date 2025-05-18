package io.taig.otter.http

import cats.data.Validated
import cats.parse.Parser
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Key
import io.taig.otter.Metadata
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.schema.*
import io.taig.otter.schema.Schema

object Http:
  sealed abstract class Header[A] extends Product with Serializable

  object Header:
    sealed abstract class Value[A] extends Http.Header[A]

    object Value:
      final case class Constant[A](self: Self.Constant[Http.Header.Value.Primitive, A]) extends Http.Header.Value[A]

      object Constant:
        given ConstantSchema[Http.Header.Value.Constant, Http.Header.Value.Primitive] =
          ConstantSchema[Self.Constant[Http.Header.Value.Primitive, *], Http.Header.Value.Primitive]
            .imapK(
              [A] => (schema: Self.Constant[Http.Header.Value.Primitive, A]) => Constant(schema)
            )([A] => (value: Http.Header.Value.Constant[A]) => value.self)

      final case class Enumeration[A](self: Self.Enumeration[Http.Header.Value.Primitive, A]) extends Http.Header.Value[A]

      object Enumeration:
        given EnumerationSchema[Http.Header.Value.Enumeration, Http.Header.Value.Primitive] =
          EnumerationSchema[Self.Enumeration[Http.Header.Value.Primitive, *], Http.Header.Value.Primitive]
            .imapK(
              [A] => (schema: Self.Enumeration[Http.Header.Value.Primitive, A]) => Enumeration(schema)
            )([A] => (value: Http.Header.Value.Enumeration[A]) => value.self)

      final case class Primitive[A](self: Self.Primitive.String[A]) extends Http.Header.Value[A]

      object Primitive:
        given PrimitiveSchema.String[Http.Header.Value.Primitive] = PrimitiveSchema
          .String[Self.Primitive.String]
          .imapK(
            [A] => (schema: Self.Primitive.String[A]) => Primitive(schema)
          )([A] => (value: Http.Header.Value.Primitive[A]) => value.self)

      final case class Union[A](self: Self.Union[Http.Header.Value, A]) extends Http.Header.Value[A]

      object Union:
        given UnionSchema[Http.Header.Value.Union, Http.Header.Value] = UnionSchema[Self.Union[Http.Header.Value, *], Http.Header.Value]
          .imapK(
            [A] => (schema: Self.Union[Http.Header.Value, A]) => Union(schema)
          )([A] => (value: Http.Header.Value.Union[A]) => value.self)

      given Schema[Http.Header.Value] with
        override def imap[A, B](fa: Http.Header.Value[A])(f: A => B)(g: B => A): Http.Header.Value[B] = fa match
          case Constant(self)    => Constant(self.imap(f)(g))
          case Enumeration(self) => Enumeration(self.imap(f)(g))
          case Primitive(self)   => Primitive(self.imap(f)(g))
          case Union(self)       => Union(self.imap(f)(g))

        extension [A](self: Http.Header.Value[A])
          override def metadata: Metadata = self match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case Primitive(self)   => self.metadata
            case Union(self)       => self.metadata

          override def modifyMetadata(f: Metadata => Metadata): Http.Header.Value[A] = self match
            case Constant(self)    => Constant(self.modifyMetadata(f))
            case Enumeration(self) => Enumeration(self.modifyMetadata(f))
            case Primitive(self)   => Primitive(self.modifyMetadata(f))
            case Union(self)       => Union(self.modifyMetadata(f))

        //

    sealed abstract class Array[A] extends Http.Header[A]

    object Array:
      final case class Collection[A](self: Self.Collection[Http.Header.Value, A]) extends Http.Header.Array[A]

      object Collection:
        given CollectionSchema[Http.Header.Array.Collection, Http.Header.Value] = ???

      final case class Tuple[A](self: Self.Tuple[Http.Header.Value, A]) extends Http.Header.Array[A]

      object Tuple:
        given TupleSchema[Http.Header.Array.Tuple, Http.Header.Value] = ???

      given Schema[Http.Header.Array] = ???

    sealed abstract class Object[A] extends Http.Header[A]

    object Object:
      final case class Dictionary[A](self: Self.Dictionary[Key, Http.Header.Object.Value, A])
          extends Http.Header.Object[A]

      object Dictionary:
        given DictionarySchema[Http.Header.Object.Dictionary, Key, Http.Header.Value] = ???

      final case class Record[A](self: Self.Record[Http.Header.Field, A]) extends Http.Header.Object[A]

      object Record:
        given RecordSchema[Http.Header.Object.Record, Http.Header.Field] = ???

      type Value[A] = Self.Nullable[Http.Header.Value, A] | Http.Header.Value[A]

      given Schema[Http.Header.Object] = ???

    final case class Field[A](self: Self.Field[Key, Http.Header.Object.Value, A])

    object Field:
      given FieldSchema[Http.Header.Field, Key, Http.Header.Object.Value] = ???

  sealed abstract class Query[A] extends Product with Serializable

  object Query:
    sealed abstract class Value[A] extends Http.Query[A]

    object Value:
      final case class Constant[A](self: Self.Constant[Http.Query.Value.Primitive, A]) extends Value[A]

      object Constant:
        given ConstantSchema[Http.Query.Value.Constant, Http.Query.Value.Primitive] = ???

      final case class Enumeration[A](self: Self.Enumeration[Http.Query.Value.Primitive, A]) extends Value[A]

      object Enumeration:
        given EnumerationSchema[Http.Query.Value.Enumeration, Http.Query.Value.Primitive] = ???

      final case class Primitive[A](self: Self.Primitive.String[A]) extends Value[A]

      object Primitive:
        given PrimitiveSchema.String[Http.Query.Value.Primitive] = ???

      final case class Union[A](self: Self.Union[Http.Query.Value, A]) extends Value[A]

      object Union:
        given UnionSchema[Http.Query.Value.Union, Http.Query.Value] = ???

      given Schema[Http.Query.Value] = ???

    sealed abstract class Array[A] extends Http.Query[A]

    object Array:
      final case class Collection[A](self: Self.Collection[Http.Query.Value, A]) extends Http.Query.Array[A]

      object Collection:
        given CollectionSchema[Http.Query.Array.Collection, Http.Query.Value] = ???

      final case class Tuple[A](self: Self.Tuple[Http.Query.Value, A]) extends Http.Query.Array[A]

      object Tuple:
        given TupleSchema[Http.Query.Array.Tuple, Http.Query.Value] = ???

      given Schema[Http.Query.Array] = ???

    final case class Nullable[A](self: Self.Nullable[Http.Query, A]) extends Http.Query[A]

    object Nullable:
      given NullableSchema[Http.Query.Nullable, Http.Query] = ???

    final case class Field[A](self: Self.Field[Http.Query.Value, Http.Query.Value, A])

    object Field:
      given FieldSchema[Http.Query.Field, Http.Query.Value, Http.Query.Value] = ???

  sealed abstract class Parameter[A] extends Product with Serializable

  object Parameter:
    sealed abstract class Value[A] extends Http.Parameter[A]

    object Value:
      final case class Constant[A](self: Self.Constant[Http.Parameter.Value.Primitive, A]) extends Value[A]

      object Constant:
        given ConstantSchema[Http.Parameter.Value.Constant, Http.Parameter.Value.Primitive] = ???

      final case class Enumeration[A](self: Self.Enumeration[Http.Parameter.Value.Primitive, A]) extends Value[A]

      object Enumeration:
        given EnumerationSchema[Http.Parameter.Value.Enumeration, Http.Parameter.Value.Primitive] = ???

      final case class Primitive[A](self: Self.Primitive.String[A]) extends Value[A]

      object Primitive:
        given PrimitiveSchema.String[Http.Parameter.Value.Primitive] = ???

      final case class Union[A](self: Self.Union[Http.Parameter.Value, A]) extends Value[A]

      object Union:
        given UnionSchema[Http.Parameter.Value.Union, Http.Parameter.Value] = ???

      given Schema[Http.Parameter.Value] with
        override def imap[A, B](fa: Value[A])(f: A => B)(g: B => A): Value[B] = fa match
          case Constant(self)    => Constant(self.imap(f)(g))
          case Enumeration(self) => Enumeration(self.imap(f)(g))
          case Primitive(self)   => Primitive(self.imap(f)(g))
          case Union(self)       => Union(self.imap(f)(g))

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

    sealed abstract class Array[A] extends Http.Parameter[A]

    object Array:
      final case class Collection[A](self: Self.Collection[Http.Parameter.Value, A]) extends Http.Parameter.Array[A]

      object Collection:
        given CollectionSchema[Http.Parameter.Array.Collection, Http.Parameter.Value] = ???

      final case class Tuple[A](self: Self.Tuple[Http.Parameter.Value, A]) extends Http.Parameter.Array[A]

      object Tuple:
        given TupleSchema[Http.Parameter.Array.Tuple, Http.Parameter.Value] = ???

      given Schema[Http.Parameter.Array] with
        override def imap[A, B](fa: Array[A])(f: A => B)(g: B => A): Array[B] = fa match
          case Collection(self) => Collection(self.imap(f)(g))
          case Tuple(self)      => Tuple(self.imap(f)(g))

        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def modifyMetadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.modifyMetadata(f))
            case Tuple(self)      => Tuple(self.modifyMetadata(f))

    sealed abstract class Object[A] extends Http.Parameter[A]

    object Object:
      final case class Dictionary[A](self: Self.Dictionary[Http.Parameter.Value, Http.Parameter.Value, A])
          extends Http.Parameter.Object[A]

      object Dictionary:
        given DictionarySchema[Http.Parameter.Object.Dictionary, Http.Parameter.Value, Http.Parameter.Value] = ???

      final case class Record[A](self: Self.Record[Http.Parameter.Field, A]) extends Http.Parameter.Object[A]

      object Record:
        given RecordSchema[Http.Parameter.Object.Record, Http.Parameter.Field] = ???

      given Schema[Http.Parameter.Object] with
        override def imap[A, B](fa: Http.Parameter.Object[A])(f: A => B)(g: B => A): Object[B] =
          fa match
            case Dictionary(self) => Dictionary(self.imap(f)(g))
            case Record(self)     => Record(self.imap(f)(g))

        extension [A](self: Object[A])
          override def metadata: Metadata = self match
            case Dictionary(self) => self.metadata
            case Record(self)     => self.metadata

          override def modifyMetadata(f: Metadata => Metadata): Object[A] = self match
            case Dictionary(self) => Dictionary(self.modifyMetadata(f))
            case Record(self)     => Record(self.modifyMetadata(f))

    final case class Field[A](self: Self.Field[Http.Parameter.Value, Http.Parameter.Value, A])

    object Field:
      given FieldSchema[Http.Parameter.Field, Http.Parameter.Value, Http.Parameter.Value] = ???

extension [A](self: Either[Parser.Error, A])
  private[otter] def toValidatedViolations(tpe: String, value: String): Validated[Violations, A] =
    self.toValidated.leftMap: error =>
      Violations.rootNec(Violation.tpe(name = tpe, actual = value, hint = error.show))
