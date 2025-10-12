package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.*
import cats.Invariant
import cats.derived.*

sealed abstract class Json[+S[a] <: Json[?, a], A] extends Product with Serializable derives Invariant

object Json:
  final case class Coerce[+S[a] <: Json.Primitive[a], A](self: Annotation[Self.Coerce[S, A]]) extends Json[S, A]
      derives Invariant

  object Coerce:
    given [S[a] <: Json.Primitive[a], A]: Annotated[Coerce[S, A]] =
      Annotated[Annotation[Self.Coerce[S, A]]].imap(Coerce.apply)(_.self)

    given CoerceOperation[Json.Primitive, Json.Coerce] =
      CoerceOperation[Json.Primitive, [s[a] <: Json.Primitive[a], a] =>> Annotation[Self.Coerce[s, a]]]
        .imapK[Json.Coerce]([Value[a] <: Json.Primitive[a], A] =>
          (self: Annotation[Self.Coerce[Value, A]]) => Coerce(self)
        )([Value[a] <: Json.Primitive[a], A] => (schema: Json.Coerce[Value, A]) => schema.self)

  final case class Collection[+S[a] <: Json[?, a], A](self: Annotation[Self.Collection[S, A]]) extends Json[S, A]
      derives Invariant

  object Collection:
    given [S[a] <: Json[?, a], A]: Annotated[Json.Collection[S, A]] =
      Annotated[Annotation[Self.Collection[S, A]]].imap(Collection.apply)(_.self)

    given CollectionOperation[Json[?, *], Json.Collection] =
      CollectionOperation[Json[?, *], [s[a] <: Json[?, a], a] =>> Annotation[Self.Collection[s, a]]]
        .imapK([Value[a] <: Json[?, a], A] => (self: Annotation[Self.Collection[Value, A]]) => Collection(self))(
          [Value[a] <: Json[?, a], A] => (schema: Json.Collection[Value, A]) => schema.self
        )

  final case class Constant[+S[a] <: Json.Primitive[a], A](self: Annotation[Self.Constant[S, A]]) extends Json[S, A]
      derives Invariant

  object Constant:
    given [S[a] <: Json.Primitive[a], A]: Annotated[Json.Constant[S, A]] =
      Annotated[Annotation[Self.Constant[S, A]]].imap(Constant.apply)(_.self)

    given ConstantOperation[Json.Primitive, Json.Constant] =
      ConstantOperation[Json.Primitive, [s[a] <: Json.Primitive[a], a] =>> Annotation[Self.Constant[s, a]]]
        .imapK[Json.Constant]([Value[a] <: Json.Primitive[a], A] =>
          (self: Annotation[Self.Constant[Value, A]]) => Constant(self)
        )([Value[a] <: Json.Primitive[a], A] => (schema: Json.Constant[Value, A]) => schema.self)

  final case class Dictionary[+S[a] <: Json[?, a], A](self: Annotation[Self.Dictionary[S, A]]) extends Json[S, A]
      derives Invariant

  object Dictionary:
    given [S[a] <: Json[?, a], A]: Annotated[Json.Dictionary[S, A]] =
      Annotated[Annotation[Self.Dictionary[S, A]]].imap(Dictionary.apply)(_.self)

    given DictionaryOperation[Json[?, *], Json.Dictionary] =
      DictionaryOperation[Json[?, *], [s[a] <: Json[?, a], a] =>> Annotation[Self.Dictionary[s, a]]]
        .imapK([Value[a] <: Json[?, a], A] => (self: Annotation[Self.Dictionary[Value, A]]) => Dictionary(self))(
          [Value[a] <: Json[?, a], A] => (schema: Json.Dictionary[Value, A]) => schema.self
        )

  final case class Enumeration[+S[a] <: Json.Primitive[a], A](self: Annotation[Self.Enumeration[S, A]])
      extends Json[S, A] derives Invariant

  object Enumeration:
    given [S[a] <: Json.Primitive[a], A]: Annotated[Json.Enumeration[S, A]] =
      Annotated[Annotation[Self.Enumeration[S, A]]].imap(Enumeration.apply)(_.self)

    given EnumerationOperation[Json.Primitive, Json.Enumeration] =
      EnumerationOperation[Json.Primitive, [s[a] <: Json.Primitive[a], a] =>> Annotation[Self.Enumeration[s, a]]]
        .imapK[Json.Enumeration]([Value[a] <: Json.Primitive[a], A] =>
          (self: Annotation[Self.Enumeration[Value, A]]) => Enumeration(self)
        )([Value[a] <: Json.Primitive[a], A] => (schema: Enumeration[Value, A]) => schema.self)

  final case class Nullable[+S[a] <: Json[?, a], A](self: Annotation[Self.Nullable[S, A]]) extends Json[S, A]
      derives Invariant

  object Nullable:
    given [S[a] <: Json[?, a], A]: Annotated[Json.Nullable[S, A]] =
      Annotated[Annotation[Self.Nullable[S, A]]].imap(Nullable.apply)(_.self)

    given NullableOperation[Json[?, *], Json.Nullable] =
      NullableOperation[Json[?, *], [s[a] <: Json[?, a], a] =>> Annotation[Self.Nullable[s, a]]]
        .imapK[Json.Nullable]([Value[a] <: Json[?, a], A] =>
          (self: Annotation[Self.Nullable[Value, A]]) => Nullable(self)
        )([Value[a] <: Json[?, a], A] => (schema: Json.Nullable[Value, A]) => schema.self)

  sealed abstract class Primitive[A] extends Json[Nothing, A] derives Invariant:
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

  final case class Record[+S[a] <: Json[?, a], A](self: Annotation[Self.Record[Json.Field[S, *], A]]) extends Json[S, A]
      derives Invariant

  object Record:
    given [S[a] <: Json[?, a], A]: Annotated[Json.Record[S, A]] =
      Annotated[Annotation[Self.Record[Json.Field[S, *], A]]].imap(Record.apply)(_.self)

    given RecordOperation[Json[?, *], Json.Record] =
      // RecordOperation[
      //   Json[?, *],
      //   [s[a] <: Json[?, a], a] =>> Annotation[Self.Record[Json.Field[s, *], a]]
      // ]

      ???
      // RecordOperation[Json[?, *], [s[a] <: Json[?, a], a] =>> Annotation[Self.Record[s, a]]]
      //   .imapK([Value[a] <: Json[?, a], A] => (self: Annotation[Self.Record[Value, A]]) => Record(self))(
      //     [Value[a] <: Json[?, a], A] => (schema: Json.Record[Value, A]) => schema.self
      //   )

  final case class Tuple[+S[a] <: Json[?, a], A](self: Annotation[Self.Tuple[S, A]]) extends Json[S, A]
      derives Invariant

  object Tuple:
    given [S[a] <: Json[?, a], A]: Annotated[Json.Tuple[S, A]] =
      Annotated[Annotation[Self.Tuple[S, A]]].imap(Tuple.apply)(_.self)

    given TupleOperation[Json[?, *], Json.Tuple] =
      TupleOperation[Json[?, *], [s[a] <: Json[?, a], a] =>> Annotation[Self.Tuple[s, a]]]
        .imapK([Value[a] <: Json[?, a], A] => (self: Annotation[Self.Tuple[Value, A]]) => Tuple(self))(
          [Value[a] <: Json[?, a], A] => (schema: Json.Tuple[Value, A]) => schema.self
        )

  final case class Union[+S[a] <: Json[?, a], A](self: Annotation[Self.Union[S, A]]) extends Json[S, A]
      derives Invariant

  object Union:
    given [S[a] <: Json[?, a], A]: Annotated[Json.Union[S, A]] =
      Annotated[Annotation[Self.Union[S, A]]].imap(Union.apply)(_.self)

    given UnionOperation[Json[?, *], Json.Union] =
      UnionOperation[Json[?, *], [s[a] <: Json[?, a], a] =>> Annotation[Self.Union[s, a]]]
        .imapK([Value[a] <: Json[?, a], A] => (self: Annotation[Self.Union[Value, A]]) => Union(self))(
          [Value[a] <: Json[?, a], A] => (schema: Json.Union[Value, A]) => schema.self
        )

  final case class Field[+S[a] <: Json[?, a], A](self: Annotation[Self.Field[S, A]]) derives Invariant

  object Field:
    given [S[a] <: Json[?, a], A]: Annotated[Json.Field[S, A]] =
      Annotated[Annotation[Self.Field[S, A]]].imap(Field.apply)(_.self)

    given FieldOperation[Json[?, *], Json.Field] =
      FieldOperation[Json[?, *], [s[a] <: Json[?, a], a] =>> Annotation[Self.Field[s, a]]]
        .imapK([Value[a] <: Json[?, a], A] => (self: Annotation[Self.Field[Value, A]]) => Field(self))(
          [Value[a] <: Json[?, a], A] => (schema: Json.Field[Value, A]) => schema.self
        )
