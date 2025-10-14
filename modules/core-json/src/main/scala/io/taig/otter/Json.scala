package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.*
import cats.Invariant
import cats.derived.*

sealed abstract class Json[A] extends Product with Serializable derives Invariant

object Json:
  final case class Coerce[A](self: Annotation[Self.Coerce[Json.Primitive, A]]) extends Json[A] derives Invariant

  object Coerce:
    given [A]: Annotated[Coerce[A]] =
      Annotated[Annotation[Self.Coerce[Json.Primitive, A]]].imap(Coerce.apply)(_.self)

    given CoerceOperation[Json.Coerce, Json.Primitive] =
      CoerceOperation[[a] =>> Annotation[Self.Coerce[Json.Primitive, a]], Json.Primitive]
        .imapK[Json.Coerce]([A] => (self: Annotation[Self.Coerce[Json.Primitive, A]]) => Coerce(self))([A] =>
          (schema: Json.Coerce[A]) => schema.self
        )

  final case class Collection[A](self: Annotation[Self.Collection[Json, A]]) extends Json[A] derives Invariant

  object Collection:
    given [A]: Annotated[Json.Collection[A]] =
      Annotated[Annotation[Self.Collection[Json, A]]].imap(Collection.apply)(_.self)

    given CollectionOperation[Json.Collection, Json] =
      CollectionOperation[[a] =>> Annotation[Self.Collection[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Collection[Json, A]]) => Collection(self))([A] =>
          (schema: Json.Collection[A]) => schema.self
        )

  final case class Constant[A](self: Annotation[Self.Constant[Json.Primitive, A]]) extends Json[A] derives Invariant

  object Constant:
    given [A]: Annotated[Json.Constant[A]] =
      Annotated[Annotation[Self.Constant[Json.Primitive, A]]].imap(Constant.apply)(_.self)

    given ConstantOperation[Json.Constant, Json.Primitive] =
      ConstantOperation[[a] =>> Annotation[Self.Constant[Json.Primitive, a]], Json.Primitive]
        .mapK([A] => (self: Annotation[Self.Constant[Json.Primitive, A]]) => Constant(self))

  final case class Dictionary[A](self: Annotation[Self.Dictionary[Json, A]]) extends Json[A] derives Invariant

  object Dictionary:
    given [A]: Annotated[Json.Dictionary[A]] =
      Annotated[Annotation[Self.Dictionary[Json, A]]].imap(Dictionary.apply)(_.self)

    given DictionaryOperation[Json.Dictionary, Json] =
      DictionaryOperation[[a] =>> Annotation[Self.Dictionary[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Dictionary[Json, A]]) => Dictionary(self))([A] =>
          (schema: Json.Dictionary[A]) => schema.self
        )

  final case class Enumeration[A](self: Annotation[Self.Enumeration[Json.Primitive, A]]) extends Json[A]
      derives Invariant

  object Enumeration:
    given [A]: Annotated[Json.Enumeration[A]] =
      Annotated[Annotation[Self.Enumeration[Json.Primitive, A]]].imap(Enumeration.apply)(_.self)

    given EnumerationOperation[Json.Enumeration, Json.Primitive] =
      EnumerationOperation[[a] =>> Annotation[Self.Enumeration[Json.Primitive, a]], Json.Primitive]
        .mapK([A] => (self: Annotation[Self.Enumeration[Json.Primitive, A]]) => Enumeration(self))

  final case class Nullable[A](self: Annotation[Self.Nullable[Json, A]]) extends Json[A] derives Invariant

  object Nullable:
    given [A]: Annotated[Json.Nullable[A]] =
      Annotated[Annotation[Self.Nullable[Json, A]]].imap(Nullable.apply)(_.self)

    given NullableOperation[Json.Nullable, Json] =
      NullableOperation[[a] =>> Annotation[Self.Nullable[Json, a]], Json]
        .mapK([A] => (self: Annotation[Self.Nullable[Json, A]]) => Nullable(self))

  sealed abstract class Primitive[A] extends Json[A] derives Invariant:
    def self: Annotation[Self.Primitive[A]]

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Json.Primitive[A] derives Invariant

    object Boolean:
      given [A]: Annotated[Json.Primitive.Boolean[A]] = Annotated[Annotation[Self.Primitive.Boolean[A]]]
        .imap(Boolean.apply)(_.self)

      given BooleanOperation[Json.Primitive.Boolean] =
        BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK([A] =>
          (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
        )

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Json.Primitive[A] derives Invariant

    object Number:
      given [A]: Annotated[Json.Primitive.Number[A]] = Annotated[Annotation[Self.Primitive.Number[A]]]
        .imap(Number.apply)(_.self)

      given NumberOperation[Json.Primitive.Number] =
        NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
        )([A] => (schema: Number[A]) => schema.self)

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Json.Primitive[A] derives Invariant

    object String:
      given [A]: Annotated[Json.Primitive.String[A]] = Annotated[Annotation[Self.Primitive.String[A]]]
        .imap(String.apply)(_.self)

      given StringOperation[Json.Primitive.String] =
        StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.String[A]]) => String(self)
        )([A] => (schema: String[A]) => schema.self)

    given [A]: Annotated[Json.Primitive[A]] = Annotated[Annotation[Self.Primitive[A]]]
      .imap { self =>
        self.self match
          case schema: Self.Primitive.Boolean[A] => Boolean(self.copy(self = schema))
          case schema: Self.Primitive.Number[A]  => Number(self.copy(self = schema))
          case schema: Self.Primitive.String[A]  => String(self.copy(self = schema))
      }(_.self)

    given PrimitiveOperation[Json.Primitive] = PrimitiveOperation[[a] =>> Annotation[Self.Primitive[a]]].imapK([A] =>
      (self: Annotation[Self.Primitive[A]]) =>
        self.self match
          case schema: Self.Primitive.Boolean[A] => Json.Primitive.Boolean(self.copy(self = schema))
          case schema: Self.Primitive.Number[A]  => Json.Primitive.Number(self.copy(self = schema))
          case schema: Self.Primitive.String[A]  => Json.Primitive.String(self.copy(self = schema))
    )([A] =>
      (schema: Json.Primitive[A]) =>
        schema match
          case Json.Primitive.Boolean(self) => self
          case Json.Primitive.Number(self)  => self
          case Json.Primitive.String(self)  => self
    )

  final case class Record[A](self: Annotation[Self.Record[Json.Field, A]]) extends Json[A] derives Invariant

  object Record:
    given [A]: Annotated[Json.Record[A]] =
      Annotated[Annotation[Self.Record[Json.Field, A]]].imap(Record.apply)(_.self)

    given RecordOperation[Json.Record, Json.Field] =
      RecordOperation[[a] =>> Annotation[Self.Record[Json.Field, a]], Json.Field].imapK([A] =>
        (self: Annotation[Self.Record[Json.Field, A]]) => Record(self)
      )([A] => (schema: Json.Record[A]) => schema.self)

  final case class Tuple[A](self: Annotation[Self.Tuple[Json, A]]) extends Json[A] derives Invariant

  object Tuple:
    given [A]: Annotated[Json.Tuple[A]] =
      Annotated[Annotation[Self.Tuple[Json, A]]].imap(Tuple.apply)(_.self)

    given TupleOperation[Json.Tuple, Json] = TupleOperation[[a] =>> Annotation[Self.Tuple[Json, a]], Json]
      .imapK([A] => (self: Annotation[Self.Tuple[Json, A]]) => Tuple(self))([A] =>
        (schema: Json.Tuple[A]) => schema.self
      )

  final case class Union[A](self: Annotation[Self.Union[Json, A]]) extends Json[A] derives Invariant

  object Union:
    given [A]: Annotated[Json.Union[A]] =
      Annotated[Annotation[Self.Union[Json, A]]].imap(Union.apply)(_.self)

    given UnionOperation[Json.Union, Json] =
      UnionOperation[[a] =>> Annotation[Self.Union[Json, a]], Json].imapK([A] =>
        (self: Annotation[Self.Union[Json, A]]) => Union(self)
      )([A] => (schema: Json.Union[A]) => schema.self)

  final case class Field[A](self: Annotation[Self.Field[Json, A]]) derives Invariant

  object Field:
    given [A]: Annotated[Json.Field[A]] = Annotated[Annotation[Self.Field[Json, A]]].imap(Field.apply)(_.self)

    given FieldOperation[Json.Field, Json] = FieldOperation[[a] =>> Annotation[Self.Field[Json, a]], Json]
      .imapK([A] => (self: Annotation[Self.Field[Json, A]]) => Field(self))([A] =>
        (schema: Json.Field[A]) => schema.self
      )
