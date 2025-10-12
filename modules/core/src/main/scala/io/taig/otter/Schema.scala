package io.taig.otter

import io.taig.otter as Self
import cats.Invariant
import cats.derived.*
import io.taig.otter.operation.*

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Product with Serializable

object Schema:
  final case class Coerce[+S[a] <: Schema[?, a], A](self: Annotation[Self.Coerce[S, A]]) extends Schema[S, A]
      derives Invariant

  object Coerce:
    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Coerce[S, A]] =
      Annotated[Annotation[Self.Coerce[S, A]]].imap(Coerce.apply)(_.self)

    given CoerceOperation[Schema[?, *], Schema.Coerce] =
      CoerceOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Coerce[s, a]]]
        .imapK[Schema.Coerce]([Value[a] <: Schema[?, a], A] =>
          (self: Annotation[Self.Coerce[Value, A]]) => Coerce(self)
        )([Value[a] <: Schema[?, a], A] => (schema: Coerce[Value, A]) => schema.self)

  final case class Collection[+S[a] <: Schema[?, a], A](self: Annotation[Self.Collection[S, A]]) extends Schema[S, A]
      derives Invariant

  object Collection:
    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Collection[S, A]] =
      Annotated[Annotation[Self.Collection[S, A]]].imap(Collection.apply)(_.self)

    given CollectionOperation[Schema[?, *], Schema.Collection] =
      CollectionOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Collection[s, a]]]
        .imapK([Value[a] <: Schema[?, a], A] => (self: Annotation[Self.Collection[Value, A]]) => Collection(self))(
          [Value[a] <: Schema[?, a], A] => (schema: Collection[Value, A]) => schema.self
        )

  final case class Constant[+S[a] <: Schema[?, a], A](self: Annotation[Self.Constant[S, A]]) extends Schema[S, A]
      derives Invariant

  object Constant:
    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Constant[S, A]] =
      Annotated[Annotation[Self.Constant[S, A]]].imap(Constant.apply)(_.self)

    given ConstantOperation[Schema[?, *], Schema.Constant] =
      ConstantOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Constant[s, a]]]
        .imapK[Schema.Constant]([Value[a] <: Schema[?, a], A] =>
          (self: Annotation[Self.Constant[Value, A]]) => Constant(self)
        )([Value[a] <: Schema[?, a], A] => (schema: Constant[Value, A]) => schema.self)

  final case class Dictionary[+S[a] <: Schema[?, a], A](self: Annotation[Self.Dictionary[S, A]]) extends Schema[S, A]
      derives Invariant

  object Dictionary:
    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Dictionary[S, A]] =
      Annotated[Annotation[Self.Dictionary[S, A]]].imap(Dictionary.apply)(_.self)

    given DictionaryOperation[Schema[?, *], Schema.Dictionary] =
      DictionaryOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Dictionary[s, a]]]
        .imapK([Value[a] <: Schema[?, a], A] => (self: Annotation[Self.Dictionary[Value, A]]) => Dictionary(self))(
          [Value[a] <: Schema[?, a], A] => (schema: Dictionary[Value, A]) => schema.self
        )

  final case class Enumeration[+S[a] <: Schema[?, a], A](self: Annotation[Self.Enumeration[S, A]]) extends Schema[S, A]
      derives Invariant

  object Enumeration:
    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Enumeration[S, A]] =
      Annotated[Annotation[Self.Enumeration[S, A]]].imap(Enumeration.apply)(_.self)

    given EnumerationOperation[Schema[?, *], Schema.Enumeration] =
      EnumerationOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Enumeration[s, a]]]
        .imapK[Schema.Enumeration]([Value[a] <: Schema[?, a], A] =>
          (self: Annotation[Self.Enumeration[Value, A]]) => Enumeration(self)
        )([Value[a] <: Schema[?, a], A] => (schema: Enumeration[Value, A]) => schema.self)

  final case class Nullable[+S[a] <: Schema[?, a], A](self: Annotation[Self.Nullable[S, A]]) extends Schema[S, A]
      derives Invariant

  object Nullable:
    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Nullable[S, A]] =
      Annotated[Annotation[Self.Nullable[S, A]]].imap(Nullable.apply)(_.self)

    given NullableOperation[Schema[?, *], Schema.Nullable] =
      NullableOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Nullable[s, a]]]
        .imapK[Schema.Nullable]([Value[a] <: Schema[?, a], A] =>
          (self: Annotation[Self.Nullable[Value, A]]) => Nullable(self)
        )([Value[a] <: Schema[?, a], A] => (schema: Nullable[Value, A]) => schema.self)

  sealed abstract class Primitive[A] extends Schema[Nothing, A]

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Schema.Primitive[A]
        derives Invariant

    object Boolean:
      given [A]: Annotated[Schema.Primitive.Boolean[A]] =
        Annotated[Annotation[Self.Primitive.Boolean[A]]].imap(Boolean.apply)(_.self)

      given BooleanOperation[Schema.Primitive.Boolean] =
        BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK([A] =>
          (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
        )

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Schema.Primitive[A] derives Invariant

    object Number:
      given [A]: Annotated[Schema.Primitive.Number[A]] =
        Annotated[Annotation[Self.Primitive.Number[A]]].imap(Number.apply)(_.self)

      given NumberOperation[Schema.Primitive.Number] =
        NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
        )([A] => (schema: Schema.Primitive.Number[A]) => schema.self)

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Schema.Primitive[A] derives Invariant

    object String:
      given [A]: Annotated[Schema.Primitive.String[A]] =
        Annotated[Annotation[Self.Primitive.String[A]]].imap(String.apply)(_.self)

      given StringOperation[Schema.Primitive.String] =
        StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.String[A]]) => String(self)
        )([A] => (schema: Schema.Primitive.String[A]) => schema.self)

    given [A]: Annotated[Schema.Primitive[A]] =
      Annotated[Annotation[Self.Primitive[A]]].imap { self =>
        self.self match
          case schema: Self.Primitive.Boolean[A] => Boolean(self.copy(self = schema))
          case schema: Self.Primitive.Number[A]  => Number(self.copy(self = schema))
          case schema: Self.Primitive.String[A]  => String(self.copy(self = schema))
      } {
        case Schema.Primitive.Boolean(self) => self
        case Schema.Primitive.Number(self)  => self
        case Schema.Primitive.String(self)  => self
      }

  final case class Record[+S[a] <: Schema[?, a], A](self: Annotation[Self.Record[Schema.Field[S, *], A]])
      extends Schema[S, A] derives Invariant

  object Record:
    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Record[S, A]] =
      Annotated[Annotation[Self.Record[Schema.Field[S, *], A]]].imap(Record.apply)(_.self)

    given RecordOperation[Schema.Field[?, *], Schema.Record] = new RecordOperation[Schema.Field[?, *], Schema.Record] {
      override def empty: Record[Nothing, Unit] = Record(Annotation(Self.Record.Empty))

      override def lift[Value[a] <: Schema.Field[?, a], A](value: => Value[A]): Record[Value, A] = ???

      extension [S[a] <: Schema[?, a], A](self: Record[S, A])
        override def zip[T[a] <: Schema[?, a], B](schema: Record[T, B]): Record[[a] =>> S[a] | T[a], (A, B)] = ???
    }

  final case class Tuple[+S[a] <: Schema[?, a], A](self: Annotation[Self.Tuple[S, A]]) extends Schema[S, A]
      derives Invariant

  object Tuple:
    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Tuple[S, A]] =
      Annotated[Annotation[Self.Tuple[S, A]]].imap(Tuple.apply)(_.self)

    given TupleOperation[Schema[?, *], Schema.Tuple] =
      TupleOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Tuple[s, a]]]
        .imapK([Value[a] <: Schema[?, a], A] => (self: Annotation[Self.Tuple[Value, A]]) => Tuple(self))(
          [Value[a] <: Schema[?, a], A] => (schema: Tuple[Value, A]) => schema.self
        )

  final case class Union[+S[a] <: Schema[?, a], A](self: Annotation[Self.Union[S, A]]) extends Schema[S, A]
      derives Invariant

  object Union:
    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Union[S, A]] =
      Annotated[Annotation[Self.Union[S, A]]].imap(Union.apply)(_.self)

    given UnionOperation[Schema[?, *], Schema.Union] =
      UnionOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Union[s, a]]]
        .imapK([Value[a] <: Schema[?, a], A] => (self: Annotation[Self.Union[Value, A]]) => Union(self))(
          [Value[a] <: Schema[?, a], A] => (schema: Union[Value, A]) => schema.self
        )

  final case class Field[+S[a] <: Schema[?, a], A](self: Annotation[Self.Field[S, A]]) extends Schema[S, A]
      derives Invariant

  object Field:
    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Field[S, A]] =
      Annotated[Annotation[Self.Field[S, A]]].imap(Field.apply)(_.self)

    given FieldOperation[Schema[?, *], Schema.Field] =
      FieldOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Field[s, a]]]
        .imapK([Value[a] <: Schema[?, a], A] => (self: Annotation[Self.Field[Value, A]]) => Field(self))(
          [Value[a] <: Schema[?, a], A] => (schema: Field[Value, A]) => schema.self
        )
