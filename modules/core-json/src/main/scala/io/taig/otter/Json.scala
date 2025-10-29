package io.taig.otter

import cats.Invariant
import cats.derived.*
import io.taig.otter as Self
import io.taig.otter.operation.*

sealed abstract class Json[A] extends Product with Serializable derives Invariant

object Json:
  final case class Coerce[A](annotation: Annotation[Self.Coerce[Json.Primitive, A]]) extends Json[A] derives Invariant

  object Coerce:
    given [A]: Annotated[Coerce[A]] = Annotated[Annotation[Self.Coerce[Json.Primitive, A]]]
      .imap(Coerce.apply)(_.annotation)

    given CoerceOperation[Json.Coerce, Json.Primitive] =
      CoerceOperation[[a] =>> Annotation[Self.Coerce[Json.Primitive, a]], Json.Primitive]
        .imapK[Json.Coerce]([A] => (self: Annotation[Self.Coerce[Json.Primitive, A]]) => Coerce(self))([A] =>
          (schema: Json.Coerce[A]) => schema.annotation
        )

  final case class Collection[A](annotation: Annotation[Self.Collection[Json, A]]) extends Json[A] derives Invariant

  object Collection:
    given [A]: Annotated[Json.Collection[A]] = Annotated[Annotation[Self.Collection[Json, A]]]
      .imap(Collection.apply)(_.annotation)

    given CollectionOperation[Json.Collection, Json] =
      CollectionOperation[[a] =>> Annotation[Self.Collection[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Collection[Json, A]]) => Collection(self))([A] =>
          (schema: Json.Collection[A]) => schema.annotation
        )

  final case class Constant[A](annotation: Annotation[Self.Constant[Json.Primitive, A]]) extends Json[A]
      derives Invariant

  object Constant:
    given [A]: Annotated[Json.Constant[A]] = Annotated[Annotation[Self.Constant[Json.Primitive, A]]]
      .imap(Constant.apply)(_.annotation)

    given ConstantOperation[Json.Constant, Json.Primitive] =
      ConstantOperation[[a] =>> Annotation[Self.Constant[Json.Primitive, a]], Json.Primitive]
        .imapK([A] => (self: Annotation[Self.Constant[Json.Primitive, A]]) => Constant(self))([A] =>
          (schema: Json.Constant[A]) => schema.annotation
        )

  final case class Dictionary[A](annotation: Annotation[Self.Dictionary[Json, A]]) extends Json[A] derives Invariant

  object Dictionary:
    given [A]: Annotated[Json.Dictionary[A]] =
      Annotated[Annotation[Self.Dictionary[Json, A]]].imap(Dictionary.apply)(_.annotation)

    given DictionaryOperation[Json.Dictionary, Json] =
      DictionaryOperation[[a] =>> Annotation[Self.Dictionary[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Dictionary[Json, A]]) => Dictionary(self))([A] =>
          (schema: Json.Dictionary[A]) => schema.annotation
        )

  final case class Enumeration[A](annotation: Annotation[Self.Enumeration[Json.Primitive, A]]) extends Json[A]
      derives Invariant

  object Enumeration:
    given [A]: Annotated[Json.Enumeration[A]] =
      Annotated[Annotation[Self.Enumeration[Json.Primitive, A]]].imap(Enumeration.apply)(_.annotation)

    given EnumerationOperation[Json.Enumeration, Json.Primitive] =
      EnumerationOperation[[a] =>> Annotation[Self.Enumeration[Json.Primitive, a]], Json.Primitive]
        .imapK([A] => (self: Annotation[Self.Enumeration[Json.Primitive, A]]) => Enumeration(self))([A] =>
          (schema: Json.Enumeration[A]) => schema.annotation
        )

  final case class Nullable[A](annotation: Annotation[Self.Nullable[Json, A]]) extends Json[A] derives Invariant

  object Nullable:
    given [A]: Annotated[Json.Nullable[A]] =
      Annotated[Annotation[Self.Nullable[Json, A]]].imap(Nullable.apply)(_.annotation)

    given NullableOperation[Json.Nullable, Json] =
      NullableOperation[[a] =>> Annotation[Self.Nullable[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Nullable[Json, A]]) => Nullable(self))([A] =>
          (schema: Json.Nullable[A]) => schema.annotation
        )

  sealed abstract class Primitive[A] extends Json[A] derives Invariant:
    def annotation: Annotation[Self.Primitive[A]]

  object Primitive:
    final case class Boolean[A](annotation: Annotation[Self.Primitive.Boolean[A]]) extends Json.Primitive[A]
        derives Invariant

    object Boolean:
      given [A]: Annotated[Json.Primitive.Boolean[A]] = Annotated[Annotation[Self.Primitive.Boolean[A]]]
        .imap(Boolean.apply)(_.annotation)

      given BooleanOperation[Json.Primitive.Boolean] =
        BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK([A] =>
          (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
        )

    final case class Number[A](annotation: Annotation[Self.Primitive.Number[A]]) extends Json.Primitive[A]
        derives Invariant

    object Number:
      given [A]: Annotated[Json.Primitive.Number[A]] = Annotated[Annotation[Self.Primitive.Number[A]]]
        .imap(Number.apply)(_.annotation)

      given NumberOperation[Json.Primitive.Number] =
        NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
        )([A] => (schema: Number[A]) => schema.annotation)

    final case class String[A](annotation: Annotation[Self.Primitive.String[A]]) extends Json.Primitive[A]
        derives Invariant

    object String:
      given [A]: Annotated[Json.Primitive.String[A]] = Annotated[Annotation[Self.Primitive.String[A]]]
        .imap(String.apply)(_.annotation)

      given StringOperation[Json.Primitive.String] =
        StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.String[A]]) => String(self)
        )([A] => (schema: String[A]) => schema.annotation)

    given [A]: Annotated[Json.Primitive[A]] = Annotated[Annotation[Self.Primitive[A]]]
      .imap { self =>
        self.self match
          case schema: Self.Primitive.Boolean[A] => Boolean(self.copy(self = schema))
          case schema: Self.Primitive.Number[A]  => Number(self.copy(self = schema))
          case schema: Self.Primitive.String[A]  => String(self.copy(self = schema))
      }(_.annotation)

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

  final case class Record[A](annotation: Annotation[Self.Record[Json.Field, A]]) extends Json[A] derives Invariant

  object Record:
    given [A]: Annotated[Json.Record[A]] =
      Annotated[Annotation[Self.Record[Json.Field, A]]].imap(Record.apply)(_.annotation)

    given RecordOperation[Json.Record, Json.Field] =
      RecordOperation[[a] =>> Annotation[Self.Record[Json.Field, a]], Json.Field].imapK([A] =>
        (self: Annotation[Self.Record[Json.Field, A]]) => Record(self)
      )([A] => (schema: Json.Record[A]) => schema.annotation)

  final case class Tuple[A](annotation: Annotation[Self.Tuple[Json, A]]) extends Json[A] derives Invariant

  object Tuple:
    given [A]: Annotated[Json.Tuple[A]] =
      Annotated[Annotation[Self.Tuple[Json, A]]].imap(Tuple.apply)(_.annotation)

    given TupleOperation[Json.Tuple, Json] = TupleOperation[[a] =>> Annotation[Self.Tuple[Json, a]], Json]
      .imapK([A] => (self: Annotation[Self.Tuple[Json, A]]) => Tuple(self))([A] =>
        (schema: Json.Tuple[A]) => schema.annotation
      )

  final case class Union[A](annotation: Annotation[Self.Union[Json, A]]) extends Json[A] derives Invariant

  object Union:
    given [A]: Annotated[Json.Union[A]] =
      Annotated[Annotation[Self.Union[Json, A]]].imap(Union.apply)(_.annotation)

    given UnionOperation[Json.Union, Json] =
      UnionOperation[[a] =>> Annotation[Self.Union[Json, a]], Json].imapK([A] =>
        (self: Annotation[Self.Union[Json, A]]) => Union(self)
      )([A] => (schema: Json.Union[A]) => schema.annotation)

  final case class Field[A](annotation: Annotation[Self.Field[Json, A]]) derives Invariant

  object Field:
    given [A]: Annotated[Json.Field[A]] = Annotated[Annotation[Self.Field[Json, A]]].imap(Field.apply)(_.annotation)

    given FieldOperation[Json.Field, Json] = FieldOperation[[a] =>> Annotation[Self.Field[Json, a]], Json]
      .imapK([A] => (annotation: Annotation[Self.Field[Json, A]]) => Field(annotation))([A] =>
        (json: Json.Field[A]) => json.annotation
      )

  given [A]: Annotated[Json[A]] with
    override def get(self: Json[A]): Metadata = self match
      case json @ Json.Coerce(_)            => Annotated[Json.Coerce[A]].get(self = json)
      case json @ Json.Collection(_)        => Annotated[Json.Collection[A]].get(self = json)
      case json @ Json.Constant(_)          => Annotated[Json.Constant[A]].get(self = json)
      case json @ Json.Dictionary(_)        => Annotated[Json.Dictionary[A]].get(self = json)
      case json @ Json.Enumeration(_)       => Annotated[Json.Enumeration[A]].get(self = json)
      case json @ Json.Nullable(_)          => Annotated[Json.Nullable[A]].get(self = json)
      case json @ Json.Primitive.Boolean(_) => Annotated[Json.Primitive.Boolean[A]].get(self = json)
      case json @ Json.Primitive.Number(_)  => Annotated[Json.Primitive.Number[A]].get(self = json)
      case json @ Json.Primitive.String(_)  => Annotated[Json.Primitive.String[A]].get(self = json)
      case json @ Json.Record(_)            => Annotated[Json.Record[A]].get(self = json)
      case json @ Json.Tuple(_)             => Annotated[Json.Tuple[A]].get(self = json)
      case json @ Json.Union(_)             => Annotated[Json.Union[A]].get(self = json)

    override def update(self: Json[A], metadata: Metadata => Metadata): Json[A] = self match
      case json @ Json.Coerce(_)            => Annotated[Json.Coerce[A]].update(self = json, metadata)
      case json @ Json.Collection(_)        => Annotated[Json.Collection[A]].update(self = json, metadata)
      case json @ Json.Constant(_)          => Annotated[Json.Constant[A]].update(self = json, metadata)
      case json @ Json.Dictionary(_)        => Annotated[Json.Dictionary[A]].update(self = json, metadata)
      case json @ Json.Enumeration(_)       => Annotated[Json.Enumeration[A]].update(self = json, metadata)
      case json @ Json.Nullable(_)          => Annotated[Json.Nullable[A]].update(self = json, metadata)
      case json @ Json.Primitive.Boolean(_) => Annotated[Json.Primitive.Boolean[A]].update(self = json, metadata)
      case json @ Json.Primitive.Number(_)  => Annotated[Json.Primitive.Number[A]].update(self = json, metadata)
      case json @ Json.Primitive.String(_)  => Annotated[Json.Primitive.String[A]].update(self = json, metadata)
      case json @ Json.Record(_)            => Annotated[Json.Record[A]].update(self = json, metadata)
      case json @ Json.Tuple(_)             => Annotated[Json.Tuple[A]].update(self = json, metadata)
      case json @ Json.Union(_)             => Annotated[Json.Union[A]].update(self = json, metadata)
