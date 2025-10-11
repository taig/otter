package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.*
import cats.Invariant
import cats.derived.*

sealed abstract class Json[A] extends Product with Serializable derives Invariant

object Json:
  final case class Coerce[A](self: Annotation[Self.Coerce[Json.Primitive, A]]) extends Json[A] derives Invariant

  final case class Collection[A](self: Annotation[Self.Collection[Json, A]]) extends Json[A]
      derives Annotated,
        Invariant

  object Collection:
    given CollectionOperation[Json.Collection, Json] = ???

  final case class Constant[A](self: Annotation[Self.Constant[Json.Primitive, A]]) extends Json[A] derives Invariant

  final case class Dictionary[A](self: Annotation[Self.Dictionary[Json, A]]) extends Json[A] derives Invariant

  final case class Enumeration[A](self: Annotation[Self.Enumeration[Json.Primitive, A]]) extends Json[A]
      derives Invariant

  final case class Nullable[A](self: Annotation[Self.Nullable[Json, A]]) extends Json[A] derives Invariant

  final case class Primitive[A](self: Annotation[Self.Primitive[A]]) extends Json[A] derives Invariant

  object Primitive:
    given BooleanOperation[Json.Primitive] = ???
    given NumberOperation[Json.Primitive] = ???
    given StringOperation[Json.Primitive] = ???

  final case class Record[A](self: Annotation[Self.Record[Json.Field, A]]) extends Json[A] derives Invariant

  final case class Tuple[A](self: Annotation[Self.Tuple[Json, A]]) extends Json[A] derives Invariant

  final case class Union[A](self: Annotation[Self.Union[Json, A]]) extends Json[A] derives Invariant

  final case class Field[A](self: Annotation[Self.Field[Json, A]]) derives Invariant

object Playground:
  import io.taig.otter.component.JsonComponent.*

  val col = collection.list(string)
