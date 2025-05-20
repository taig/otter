package io.taig.otter.http
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Key
import io.taig.otter.Metadata
import io.taig.otter.schema.*
import Self.Enriched

object Http:
  sealed trait Header[A] extends Product with Serializable

  object Header:
    sealed trait Value[A] extends Http.Header[A], Http.Header.Object.Value[A]

    object Value:
      final case class Constant[A](self: Enriched[Self.Constant[Http.Header.Value.Primitive, *], A])
          extends Http.Header.Value[A]

      object Constant:
        given ConstantSchema[Http.Header.Value.Constant, Http.Header.Value.Primitive] =
          ConstantSchema[Self.Constant[Http.Header.Value.Primitive, *], Http.Header.Value.Primitive]
            .imapK(
              [A] => (schema: Self.Constant[Http.Header.Value.Primitive, A]) => Constant(Enriched(schema))
            )([A] => (value: Http.Header.Value.Constant[A]) => value.self.self)

        given EnrichedSchema[Http.Header.Value.Constant] =
          EnrichedSchema[Enriched[Self.Constant[Http.Header.Value.Primitive, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Constant[Http.Header.Value.Primitive, *], A]) => Constant(schema)
            )([A] => (value: Http.Header.Value.Constant[A]) => value.self)

      final case class Enumeration[A](self: Enriched[Self.Enumeration[Http.Header.Value.Primitive, *], A])
          extends Http.Header.Value[A]

      object Enumeration:
        given EnumerationSchema[Http.Header.Value.Enumeration, Http.Header.Value.Primitive] =
          EnumerationSchema[Self.Enumeration[Http.Header.Value.Primitive, *], Http.Header.Value.Primitive]
            .imapK(
              [A] => (schema: Self.Enumeration[Http.Header.Value.Primitive, A]) => Enumeration(Enriched(schema))
            )([A] => (value: Http.Header.Value.Enumeration[A]) => value.self.self)

        given EnrichedSchema[Http.Header.Value.Enumeration] =
          EnrichedSchema[Enriched[Self.Enumeration[Http.Header.Value.Primitive, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Enumeration[Http.Header.Value.Primitive, *], A]) => Enumeration(schema)
            )([A] => (value: Http.Header.Value.Enumeration[A]) => value.self)

      final case class Primitive[A](self: Enriched[Self.Primitive.String, A]) extends Http.Header.Value[A]

      object Primitive:
        given PrimitiveSchema.String[Http.Header.Value.Primitive] = PrimitiveSchema
          .String[Self.Primitive.String]
          .imapK(
            [A] => (schema: Self.Primitive.String[A]) => Primitive(Enriched(schema))
          )([A] => (value: Http.Header.Value.Primitive[A]) => value.self.self)

        given EnrichedSchema[Http.Header.Value.Primitive] = EnrichedSchema[Enriched[Self.Primitive.String, *]]
          .imapK(
            [A] => (schema: Enriched[Self.Primitive.String, A]) => Primitive(schema)
          )([A] => (value: Http.Header.Value.Primitive[A]) => value.self)

      final case class Union[A](self: Enriched[Self.Union[Http.Header.Value, *], A]) extends Http.Header.Value[A]

      object Union:
        given UnionSchema[Http.Header.Value.Union, Http.Header.Value] =
          UnionSchema[Self.Union[Http.Header.Value, *], Http.Header.Value]
            .imapK(
              [A] => (schema: Self.Union[Http.Header.Value, A]) => Union(Enriched(schema))
            )([A] => (value: Http.Header.Value.Union[A]) => value.self.self)

        given EnrichedSchema[Http.Header.Value.Union] = EnrichedSchema[Enriched[Self.Union[Http.Header.Value, *], *]]
          .imapK(
            [A] => (schema: Enriched[Self.Union[Http.Header.Value, *], A]) => Union(schema)
          )([A] => (value: Http.Header.Value.Union[A]) => value.self)

      given Schema[Http.Header.Value] with
        override def imap[A, B](fa: Http.Header.Value[A])(f: A => B)(g: B => A): Http.Header.Value[B] = fa match
          case Constant(self)    => Constant(self.mapF(_.imap(f)(g)))
          case Enumeration(self) => Enumeration(self.mapF(_.imap(f)(g)))
          case Primitive(self)   => Primitive(self.mapF(_.imap(f)(g)))
          case Union(self)       => Union(self.mapF(_.imap(f)(g)))

      given EnrichedSchema[Http.Header.Value] with
        extension [A](self: Value[A])
          override def metadata: Metadata = self match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case Primitive(self)   => self.metadata
            case Union(self)       => self.metadata
          override def metadata(f: Metadata => Metadata): Value[A] = self match
            case Constant(self)    => Constant(self.copy(metadata = f(self.metadata)))
            case Enumeration(self) => Enumeration(self.copy(metadata = f(self.metadata)))
            case Primitive(self)   => Primitive(self.copy(metadata = f(self.metadata)))
            case Union(self)       => Union(self.copy(metadata = f(self.metadata)))

    sealed trait Array[A] extends Http.Header[A]

    object Array:
      final case class Collection[A](self: Enriched[Self.Collection[Http.Header.Value, *], A])
          extends Http.Header.Array[A]

      object Collection:
        given CollectionSchema[Http.Header.Array.Collection, Http.Header.Value] =
          CollectionSchema[Self.Collection[Http.Header.Value, *], Http.Header.Value]
            .imapK(
              [A] => (schema: Self.Collection[Http.Header.Value, A]) => Collection(Enriched(schema))
            )([A] => (value: Http.Header.Array.Collection[A]) => value.self.self)

        given EnrichedSchema[Http.Header.Array.Collection] =
          EnrichedSchema[Enriched[Self.Collection[Http.Header.Value, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Collection[Http.Header.Value, *], A]) => Collection(schema)
            )([A] => (value: Http.Header.Array.Collection[A]) => value.self)

      final case class Tuple[A](self: Enriched[Self.Tuple[Http.Header.Value, *], A]) extends Http.Header.Array[A]

      object Tuple:
        given TupleSchema[Http.Header.Array.Tuple, Http.Header.Value] =
          TupleSchema[Self.Tuple[Http.Header.Value, *], Http.Header.Value]
            .imapK(
              [A] => (schema: Self.Tuple[Http.Header.Value, A]) => Tuple(Enriched(schema))
            )([A] => (value: Http.Header.Array.Tuple[A]) => value.self.self)

        given EnrichedSchema[Http.Header.Array.Tuple] =
          EnrichedSchema[Enriched[Self.Tuple[Http.Header.Value, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Tuple[Http.Header.Value, *], A]) => Tuple(schema)
            )([A] => (value: Http.Header.Array.Tuple[A]) => value.self)

      given Schema[Http.Header.Array] with
        override def imap[A, B](fa: Http.Header.Array[A])(f: A => B)(g: B => A): Http.Header.Array[B] = fa match
          case Collection(self) => Collection(self.mapF(_.imap(f)(g)))
          case Tuple(self)      => Tuple(self.mapF(_.imap(f)(g)))

      given EnrichedSchema[Http.Header.Array] with
        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def metadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.copy(metadata = f(self.metadata)))
            case Tuple(self)      => Tuple(self.copy(metadata = f(self.metadata)))

    sealed trait Object[A] extends Http.Header[A]

    object Object:
      final case class Dictionary[A](self: Enriched[Self.Dictionary[Key, Http.Header.Object.Value, *], A])
          extends Http.Header.Object[A]

      object Dictionary:
        given DictionarySchema[Http.Header.Object.Dictionary, Key, Http.Header.Value] =
          DictionarySchema[Self.Dictionary[Key, Http.Header.Object.Value, *], Key, Http.Header.Value]
            .imapK(
              [A] => (schema: Self.Dictionary[Key, Http.Header.Object.Value, A]) => Dictionary(Enriched(schema))
            )([A] => (value: Http.Header.Object.Dictionary[A]) => value.self.self)

        given EnrichedSchema[Http.Header.Object.Dictionary] =
          EnrichedSchema[Enriched[Self.Dictionary[Key, Http.Header.Object.Value, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Dictionary[Key, Http.Header.Object.Value, *], A]) => Dictionary(schema)
            )([A] => (value: Http.Header.Object.Dictionary[A]) => value.self)

      final case class Record[A](self: Enriched[Self.Record[Http.Header.Field, *], A]) extends Http.Header.Object[A]

      object Record:
        given RecordSchema[Http.Header.Object.Record, Http.Header.Field] =
          RecordSchema[Self.Record[Http.Header.Field, *], Http.Header.Field]
            .imapK(
              [A] => (schema: Self.Record[Http.Header.Field, A]) => Record(Enriched(schema))
            )([A] => (value: Http.Header.Object.Record[A]) => value.self.self)

        given EnrichedSchema[Http.Header.Object.Record] =
          EnrichedSchema[Enriched[Self.Record[Http.Header.Field, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Record[Http.Header.Field, *], A]) => Record(schema)
            )([A] => (value: Http.Header.Object.Record[A]) => value.self)

      sealed trait Value[A] extends Product with Serializable

      object Value:
        final case class Nullable[A](self: Enriched[Self.Nullable[Http.Header.Object.Value, *], A])
            extends Http.Header.Object.Value[A]

        object Nullable:
          given NullableSchema[Http.Header.Object.Value.Nullable, Http.Header.Object.Value] =
            NullableSchema[Self.Nullable[Http.Header.Object.Value, *], Http.Header.Object.Value]
              .imapK(
                [A] => (schema: Self.Nullable[Http.Header.Object.Value, A]) => Nullable(Enriched(schema))
              )([A] => (value: Http.Header.Object.Value.Nullable[A]) => value.self.self)

          given EnrichedSchema[Http.Header.Object.Value.Nullable] =
            EnrichedSchema[Enriched[Self.Nullable[Http.Header.Object.Value, *], *]]
              .imapK(
                [A] => (schema: Enriched[Self.Nullable[Http.Header.Object.Value, *], A]) => Nullable(schema)
              )([A] => (value: Http.Header.Object.Value.Nullable[A]) => value.self)

      given Schema[Http.Header.Object] with
        override def imap[A, B](fa: Http.Header.Object[A])(f: A => B)(g: B => A): Object[B] =
          fa match
            case Dictionary(self) => Dictionary(self.mapF(_.imap(f)(g)))
            case Record(self)     => Record(self.mapF(_.imap(f)(g)))

      given EnrichedSchema[Http.Header.Object] with
        extension [A](self: Object[A])
          override def metadata: Metadata = self match
            case Dictionary(self) => self.metadata
            case Record(self)     => self.metadata

          override def metadata(f: Metadata => Metadata): Object[A] = self match
            case Dictionary(self) => Dictionary(self.copy(metadata = f(self.metadata)))
            case Record(self)     => Record(self.copy(metadata = f(self.metadata)))

    final case class Field[A](self: Enriched[Self.Field[Key, Http.Header.Object.Value, *], A])

    object Field:
      given FieldSchema[Http.Header.Field, Key, Http.Header.Object.Value, Http.Header.Object.Record] =
        FieldSchema[
          Self.Field[Key, Http.Header.Object.Value, *],
          Key,
          Http.Header.Object.Value,
          Http.Header.Object.Record
        ]
          .imapK(
            [A] => (schema: Self.Field[Key, Http.Header.Object.Value, A]) => Field(Enriched(schema))
          )([A] => (value: Http.Header.Field[A]) => value.self.self)

      given EnrichedSchema[Http.Header.Field] =
        EnrichedSchema[Enriched[Self.Field[Key, Http.Header.Object.Value, *], *]]
          .imapK(
            [A] => (schema: Enriched[Self.Field[Key, Http.Header.Object.Value, *], A]) => Field(schema)
          )([A] => (value: Http.Header.Field[A]) => value.self)

    given Schema[Http.Header] with
      override def imap[A, B](fa: Http.Header[A])(f: A => B)(g: B => A): Http.Header[B] = fa match
        case schema: Http.Header.Value[A]  => schema.imap(f)(g)
        case schema: Http.Header.Array[A]  => schema.imap(f)(g)
        case schema: Http.Header.Object[A] => schema.imap(f)(g)

    given EnrichedSchema[Http.Header] with
      extension [A](self: Header[A])
        override def metadata: Metadata = self match
          case schema: Http.Header.Value[A]  => schema.metadata
          case schema: Http.Header.Array[A]  => schema.metadata
          case schema: Http.Header.Object[A] => schema.metadata

        override def metadata(f: Metadata => Metadata): Header[A] = self match
          case schema: Http.Header.Value[A]  => schema.metadata(f)
          case schema: Http.Header.Array[A]  => schema.metadata(f)
          case schema: Http.Header.Object[A] => schema.metadata(f)

  sealed trait Query[A] extends Product with Serializable

  object Query:
    sealed trait Value[A] extends Http.Query[A]

    object Value:
      final case class Constant[A](self: Enriched[Self.Constant[Http.Query.Value.Primitive, *], A]) extends Value[A]

      object Constant:
        given ConstantSchema[Http.Query.Value.Constant, Http.Query.Value.Primitive] =
          ConstantSchema[Self.Constant[Http.Query.Value.Primitive, *], Http.Query.Value.Primitive]
            .imapK(
              [A] => (schema: Self.Constant[Http.Query.Value.Primitive, A]) => Constant(Enriched(schema))
            )([A] => (value: Http.Query.Value.Constant[A]) => value.self.self)

        given EnrichedSchema[Http.Query.Value.Constant] =
          EnrichedSchema[Enriched[Self.Constant[Http.Query.Value.Primitive, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Constant[Http.Query.Value.Primitive, *], A]) => Constant(schema)
            )([A] => (value: Http.Query.Value.Constant[A]) => value.self)

      final case class Enumeration[A](self: Enriched[Self.Enumeration[Http.Query.Value.Primitive, *], A])
          extends Value[A]

      object Enumeration:
        given EnumerationSchema[Http.Query.Value.Enumeration, Http.Query.Value.Primitive] =
          EnumerationSchema[Self.Enumeration[Http.Query.Value.Primitive, *], Http.Query.Value.Primitive]
            .imapK(
              [A] => (schema: Self.Enumeration[Http.Query.Value.Primitive, A]) => Enumeration(Enriched(schema))
            )([A] => (value: Http.Query.Value.Enumeration[A]) => value.self.self)

        given EnrichedSchema[Http.Query.Value.Enumeration] =
          EnrichedSchema[Enriched[Self.Enumeration[Http.Query.Value.Primitive, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Enumeration[Http.Query.Value.Primitive, *], A]) => Enumeration(schema)
            )([A] => (value: Http.Query.Value.Enumeration[A]) => value.self)

      final case class Primitive[A](self: Enriched[Self.Primitive.String, A]) extends Value[A]

      object Primitive:
        given PrimitiveSchema.String[Http.Query.Value.Primitive] =
          PrimitiveSchema
            .String[Self.Primitive.String]
            .imapK(
              [A] => (schema: Self.Primitive.String[A]) => Primitive(Enriched(schema))
            )([A] => (value: Http.Query.Value.Primitive[A]) => value.self.self)

        given EnrichedSchema[Http.Query.Value.Primitive] =
          EnrichedSchema[Enriched[Self.Primitive.String, *]]
            .imapK(
              [A] => (schema: Enriched[Self.Primitive.String, A]) => Primitive(schema)
            )([A] => (value: Http.Query.Value.Primitive[A]) => value.self)

      final case class Union[A](self: Enriched[Self.Union[Http.Query.Value, *], A]) extends Value[A]

      object Union:
        given UnionSchema[Http.Query.Value.Union, Http.Query.Value] =
          UnionSchema[Self.Union[Http.Query.Value, *], Http.Query.Value]
            .imapK(
              [A] => (schema: Self.Union[Http.Query.Value, A]) => Union(Enriched(schema))
            )([A] => (value: Http.Query.Value.Union[A]) => value.self.self)

        given EnrichedSchema[Http.Query.Value.Union] =
          EnrichedSchema[Enriched[Self.Union[Http.Query.Value, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Union[Http.Query.Value, *], A]) => Union(schema)
            )([A] => (value: Http.Query.Value.Union[A]) => value.self)

      given Schema[Http.Query.Value] with
        override def imap[A, B](fa: Http.Query.Value[A])(f: A => B)(g: B => A): Http.Query.Value[B] = fa match
          case Constant(self)    => Constant(self.mapF(_.imap(f)(g)))
          case Enumeration(self) => Enumeration(self.mapF(_.imap(f)(g)))
          case Primitive(self)   => Primitive(self.mapF(_.imap(f)(g)))
          case Union(self)       => Union(self.mapF(_.imap(f)(g)))

      given EnrichedSchema[Http.Query.Value] with
        extension [A](self: Value[A])
          override def metadata: Metadata = self match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case Primitive(self)   => self.metadata
            case Union(self)       => self.metadata

          override def metadata(f: Metadata => Metadata): Value[A] = self match
            case Constant(self)    => Constant(self.copy(metadata = f(self.metadata)))
            case Enumeration(self) => Enumeration(self.copy(metadata = f(self.metadata)))
            case Primitive(self)   => Primitive(self.copy(metadata = f(self.metadata)))
            case Union(self)       => Union(self.copy(metadata = f(self.metadata)))

    sealed trait Array[A] extends Http.Query[A]

    object Array:
      final case class Collection[A](self: Enriched[Self.Collection[Http.Query.Value, *], A])
          extends Http.Query.Array[A]

      object Collection:
        given CollectionSchema[Http.Query.Array.Collection, Http.Query.Value] =
          CollectionSchema[Self.Collection[Http.Query.Value, *], Http.Query.Value]
            .imapK(
              [A] => (schema: Self.Collection[Http.Query.Value, A]) => Collection(Enriched(schema))
            )([A] => (value: Http.Query.Array.Collection[A]) => value.self.self)

        given EnrichedSchema[Http.Query.Array.Collection] =
          EnrichedSchema[Enriched[Self.Collection[Http.Query.Value, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Collection[Http.Query.Value, *], A]) => Collection(schema)
            )([A] => (value: Http.Query.Array.Collection[A]) => value.self)

      final case class Tuple[A](self: Enriched[Self.Tuple[Http.Query.Value, *], A]) extends Http.Query.Array[A]

      object Tuple:
        given TupleSchema[Http.Query.Array.Tuple, Http.Query.Value] =
          TupleSchema[Self.Tuple[Http.Query.Value, *], Http.Query.Value]
            .imapK(
              [A] => (schema: Self.Tuple[Http.Query.Value, A]) => Tuple(Enriched(schema))
            )([A] => (value: Http.Query.Array.Tuple[A]) => value.self.self)

        given EnrichedSchema[Http.Query.Array.Tuple] =
          EnrichedSchema[Enriched[Self.Tuple[Http.Query.Value, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Tuple[Http.Query.Value, *], A]) => Tuple(schema)
            )([A] => (value: Http.Query.Array.Tuple[A]) => value.self)

      given Schema[Http.Query.Array] with
        override def imap[A, B](fa: Http.Query.Array[A])(f: A => B)(g: B => A): Http.Query.Array[B] = fa match
          case Collection(self) => Collection(self.mapF(_.imap(f)(g)))
          case Tuple(self)      => Tuple(self.mapF(_.imap(f)(g)))

      given EnrichedSchema[Http.Query.Array] with
        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def metadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.copy(metadata = f(self.metadata)))
            case Tuple(self)      => Tuple(self.copy(metadata = f(self.metadata)))

    final case class Nullable[A](self: Enriched[Self.Nullable[Http.Query, *], A]) extends Http.Query[A]

    object Nullable:
      given NullableSchema[Http.Query.Nullable, Http.Query] =
        NullableSchema[Self.Nullable[Http.Query, *], Http.Query]
          .imapK(
            [A] => (schema: Self.Nullable[Http.Query, A]) => Nullable(Enriched(schema))
          )([A] => (value: Http.Query.Nullable[A]) => value.self.self)

      given EnrichedSchema[Http.Query.Nullable] =
        EnrichedSchema[Enriched[Self.Nullable[Http.Query, *], *]]
          .imapK(
            [A] => (schema: Enriched[Self.Nullable[Http.Query, *], A]) => Nullable(schema)
          )([A] => (value: Http.Query.Nullable[A]) => value.self)

  sealed trait Parameter[A] extends Product with Serializable

  object Parameter:
    sealed trait Value[A] extends Http.Parameter[A], Http.Parameter.Object.Value[A]

    object Value:
      final case class Constant[A](self: Enriched[Self.Constant[Http.Parameter.Value.Primitive, *], A])
          extends Http.Parameter.Value[A]

      object Constant:
        given ConstantSchema[Http.Parameter.Value.Constant, Http.Parameter.Value.Primitive] =
          ConstantSchema[Self.Constant[Http.Parameter.Value.Primitive, *], Http.Parameter.Value.Primitive]
            .imapK(
              [A] => (schema: Self.Constant[Http.Parameter.Value.Primitive, A]) => Constant(Enriched(schema))
            )([A] => (value: Http.Parameter.Value.Constant[A]) => value.self.self)

        given EnrichedSchema[Http.Parameter.Value.Constant] =
          EnrichedSchema[Enriched[Self.Constant[Http.Parameter.Value.Primitive, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Constant[Http.Parameter.Value.Primitive, *], A]) => Constant(schema)
            )([A] => (value: Http.Parameter.Value.Constant[A]) => value.self)

      final case class Enumeration[A](self: Enriched[Self.Enumeration[Http.Parameter.Value.Primitive, *], A])
          extends Http.Parameter.Value[A]

      object Enumeration:
        given EnumerationSchema[Http.Parameter.Value.Enumeration, Http.Parameter.Value.Primitive] =
          EnumerationSchema[Self.Enumeration[Http.Parameter.Value.Primitive, *], Http.Parameter.Value.Primitive]
            .imapK(
              [A] => (schema: Self.Enumeration[Http.Parameter.Value.Primitive, A]) => Enumeration(Enriched(schema))
            )([A] => (value: Http.Parameter.Value.Enumeration[A]) => value.self.self)

        given EnrichedSchema[Http.Parameter.Value.Enumeration] =
          EnrichedSchema[Enriched[Self.Enumeration[Http.Parameter.Value.Primitive, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Enumeration[Http.Parameter.Value.Primitive, *], A]) => Enumeration(schema)
            )([A] => (value: Http.Parameter.Value.Enumeration[A]) => value.self)

      final case class Primitive[A](self: Enriched[Self.Primitive.String, A]) extends Http.Parameter.Value[A]

      object Primitive:
        given PrimitiveSchema.String[Http.Parameter.Value.Primitive] =
          PrimitiveSchema
            .String[Self.Primitive.String]
            .imapK(
              [A] => (schema: Self.Primitive.String[A]) => Primitive(Enriched(schema))
            )([A] => (value: Http.Parameter.Value.Primitive[A]) => value.self.self)

        given EnrichedSchema[Http.Parameter.Value.Primitive] =
          EnrichedSchema[Enriched[Self.Primitive.String, *]]
            .imapK(
              [A] => (schema: Enriched[Self.Primitive.String, A]) => Primitive(schema)
            )([A] => (value: Http.Parameter.Value.Primitive[A]) => value.self)

      final case class Union[A](self: Enriched[Self.Union[Http.Parameter.Value, *], A]) extends Http.Parameter.Value[A]

      object Union:
        given UnionSchema[Http.Parameter.Value.Union, Http.Parameter.Value] =
          UnionSchema[Self.Union[Http.Parameter.Value, *], Http.Parameter.Value]
            .imapK(
              [A] => (schema: Self.Union[Http.Parameter.Value, A]) => Union(Enriched(schema))
            )([A] => (value: Http.Parameter.Value.Union[A]) => value.self.self)

        given EnrichedSchema[Http.Parameter.Value.Union] =
          EnrichedSchema[Enriched[Self.Union[Http.Parameter.Value, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Union[Http.Parameter.Value, *], A]) => Union(schema)
            )([A] => (value: Http.Parameter.Value.Union[A]) => value.self)

      given Schema[Http.Parameter.Value] with
        override def imap[A, B](fa: Value[A])(f: A => B)(g: B => A): Value[B] = fa match
          case Constant(self)    => Constant(self.mapF(_.imap(f)(g)))
          case Enumeration(self) => Enumeration(self.mapF(_.imap(f)(g)))
          case Primitive(self)   => Primitive(self.mapF(_.imap(f)(g)))
          case Union(self)       => Union(self.mapF(_.imap(f)(g)))

      given EnrichedSchema[Http.Parameter.Value] with
        extension [A](self: Value[A])
          override def metadata: Metadata = self match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case Primitive(self)   => self.metadata
            case Union(self)       => self.metadata

          override def metadata(f: Metadata => Metadata): Value[A] = self match
            case Constant(self)    => Constant(self.copy(metadata = f(self.metadata)))
            case Enumeration(self) => Enumeration(self.copy(metadata = f(self.metadata)))
            case Primitive(self)   => Primitive(self.copy(metadata = f(self.metadata)))
            case Union(self)       => Union(self.copy(metadata = f(self.metadata)))

    sealed trait Array[A] extends Http.Parameter[A]

    object Array:
      final case class Collection[A](self: Enriched[Self.Collection[Http.Parameter.Value, *], A])
          extends Http.Parameter.Array[A]

      object Collection:
        given CollectionSchema[Http.Parameter.Array.Collection, Http.Parameter.Value] =
          CollectionSchema[Self.Collection[Http.Parameter.Value, *], Http.Parameter.Value]
            .imapK(
              [A] => (schema: Self.Collection[Http.Parameter.Value, A]) => Collection(Enriched(schema))
            )([A] => (value: Http.Parameter.Array.Collection[A]) => value.self.self)

        given EnrichedSchema[Http.Parameter.Array.Collection] =
          EnrichedSchema[Enriched[Self.Collection[Http.Parameter.Value, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Collection[Http.Parameter.Value, *], A]) => Collection(schema)
            )([A] => (value: Http.Parameter.Array.Collection[A]) => value.self)

      final case class Tuple[A](self: Enriched[Self.Tuple[Http.Parameter.Value, *], A]) extends Http.Parameter.Array[A]

      object Tuple:
        given TupleSchema[Http.Parameter.Array.Tuple, Http.Parameter.Value] =
          TupleSchema[Self.Tuple[Http.Parameter.Value, *], Http.Parameter.Value]
            .imapK(
              [A] => (schema: Self.Tuple[Http.Parameter.Value, A]) => Tuple(Enriched(schema))
            )([A] => (value: Http.Parameter.Array.Tuple[A]) => value.self.self)

        given EnrichedSchema[Http.Parameter.Array.Tuple] =
          EnrichedSchema[Enriched[Self.Tuple[Http.Parameter.Value, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Tuple[Http.Parameter.Value, *], A]) => Tuple(schema)
            )([A] => (value: Http.Parameter.Array.Tuple[A]) => value.self)

      given Schema[Http.Parameter.Array] with
        override def imap[A, B](fa: Array[A])(f: A => B)(g: B => A): Array[B] = fa match
          case Collection(self) => Collection(self.mapF(_.imap(f)(g)))
          case Tuple(self)      => Tuple(self.mapF(_.imap(f)(g)))

      given EnrichedSchema[Http.Parameter.Array] with
        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def metadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.copy(metadata = f(self.metadata)))
            case Tuple(self)      => Tuple(self.copy(metadata = f(self.metadata)))

    sealed trait Object[A] extends Http.Parameter[A]

    object Object:
      final case class Dictionary[A](self: Enriched[Self.Dictionary[Key, Http.Parameter.Value, *], A])
          extends Http.Parameter.Object[A]

      object Dictionary:
        given DictionarySchema[Http.Parameter.Object.Dictionary, Key, Http.Parameter.Value] =
          DictionarySchema[Self.Dictionary[Key, Http.Parameter.Value, *], Key, Http.Parameter.Value]
            .imapK(
              [A] => (schema: Self.Dictionary[Key, Http.Parameter.Value, A]) => Dictionary(Enriched(schema))
            )([A] => (value: Http.Parameter.Object.Dictionary[A]) => value.self.self)

        given EnrichedSchema[Http.Parameter.Object.Dictionary] =
          EnrichedSchema[Enriched[Self.Dictionary[Key, Http.Parameter.Value, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Dictionary[Key, Http.Parameter.Value, *], A]) => Dictionary(schema)
            )([A] => (value: Http.Parameter.Object.Dictionary[A]) => value.self)

      final case class Record[A](self: Enriched[Self.Record[Http.Parameter.Field, *], A])
          extends Http.Parameter.Object[A]

      object Record:
        given RecordSchema[Http.Parameter.Object.Record, Http.Parameter.Field] =
          RecordSchema[Self.Record[Http.Parameter.Field, *], Http.Parameter.Field]
            .imapK(
              [A] => (schema: Self.Record[Http.Parameter.Field, A]) => Record(Enriched(schema))
            )([A] => (value: Http.Parameter.Object.Record[A]) => value.self.self)

        given EnrichedSchema[Http.Parameter.Object.Record] =
          EnrichedSchema[Enriched[Self.Record[Http.Parameter.Field, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Record[Http.Parameter.Field, *], A]) => Record(schema)
            )([A] => (value: Http.Parameter.Object.Record[A]) => value.self)

      sealed trait Value[A] extends Product with Serializable

      object Value:
        final case class Nullable[A](self: Enriched[Self.Nullable[Http.Parameter.Object.Value, *], A])
            extends Http.Parameter.Object.Value[A]

        object Nullable:
          given NullableSchema[Http.Parameter.Object.Value.Nullable, Http.Parameter.Object.Value] =
            NullableSchema[Self.Nullable[Http.Parameter.Object.Value, *], Http.Parameter.Object.Value]
              .imapK(
                [A] => (schema: Self.Nullable[Http.Parameter.Object.Value, A]) => Nullable(Enriched(schema))
              )([A] => (value: Http.Parameter.Object.Value.Nullable[A]) => value.self.self)

          given EnrichedSchema[Http.Parameter.Object.Value.Nullable] =
            EnrichedSchema[Enriched[Self.Nullable[Http.Parameter.Object.Value, *], *]]
              .imapK(
                [A] => (schema: Enriched[Self.Nullable[Http.Parameter.Object.Value, *], A]) => Nullable(schema)
              )([A] => (value: Http.Parameter.Object.Value.Nullable[A]) => value.self)

      given Schema[Http.Parameter.Object] with
        override def imap[A, B](fa: Http.Parameter.Object[A])(f: A => B)(g: B => A): Object[B] =
          fa match
            case Dictionary(self) => Dictionary(self.mapF(_.imap(f)(g)))
            case Record(self)     => Record(self.mapF(_.imap(f)(g)))

      given EnrichedSchema[Http.Parameter.Object] with
        extension [A](self: Object[A])
          override def metadata: Metadata = self match
            case Dictionary(self) => self.metadata
            case Record(self)     => self.metadata

          override def metadata(f: Metadata => Metadata): Object[A] = self match
            case Dictionary(self) => Dictionary(self.copy(metadata = f(self.metadata)))
            case Record(self)     => Record(self.copy(metadata = f(self.metadata)))

    final case class Field[A](self: Enriched[Self.Field[Key, Http.Parameter.Object.Value, *], A])

    object Field:
      given FieldSchema[Http.Parameter.Field, Key, Http.Parameter.Object.Value, Http.Parameter.Object.Record] =
        FieldSchema[
          Self.Field[Key, Http.Parameter.Object.Value, *],
          Key,
          Http.Parameter.Object.Value,
          Http.Parameter.Object.Record
        ]
          .imapK(
            [A] => (schema: Self.Field[Key, Http.Parameter.Object.Value, A]) => Field(Enriched(schema))
          )([A] => (value: Http.Parameter.Field[A]) => value.self.self)

      given EnrichedSchema[Http.Parameter.Field] =
        EnrichedSchema[Enriched[Self.Field[Key, Http.Parameter.Object.Value, *], *]]
          .imapK(
            [A] => (schema: Enriched[Self.Field[Key, Http.Parameter.Object.Value, *], A]) => Field(schema)
          )([A] => (value: Http.Parameter.Field[A]) => value.self)

    given Schema[Http.Parameter] with
      override def imap[A, B](fa: Parameter[A])(f: A => B)(g: B => A): Parameter[B] = fa match
        case schema: Http.Parameter.Value[A]  => schema.imap(f)(g)
        case schema: Http.Parameter.Array[A]  => schema.imap(f)(g)
        case schema: Http.Parameter.Object[A] => schema.imap(f)(g)

    given EnrichedSchema[Http.Parameter] with
      extension [A](self: Parameter[A])
        override def metadata: Metadata = self match
          case schema: Http.Parameter.Value[A]  => schema.metadata
          case schema: Http.Parameter.Array[A]  => schema.metadata
          case schema: Http.Parameter.Object[A] => schema.metadata

        override def metadata(f: Metadata => Metadata): Parameter[A] = self match
          case schema: Http.Parameter.Value[A]  => schema.metadata(f)
          case schema: Http.Parameter.Array[A]  => schema.metadata(f)
          case schema: Http.Parameter.Object[A] => schema.metadata(f)
