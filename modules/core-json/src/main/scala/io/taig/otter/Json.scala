package io.taig.otter

import cats.arrow.Profunctor
import io.taig.otter as Self
import io.taig.otter.operation.*

/** The JSON schema alphabet.
  *
  * Each member wraps one format agnostic node in an [[Annotation]] and ties the recursive knot by referring back to
  * `Json`. `W` is the type this schema writes, `R` the type it reads.
  */
sealed abstract class Json[-W, +R]

object Json:
  /** A schema that at least writes `A`. */
  type Write[-A] = Json[A, Any]

  /** A schema that at least reads `A`. */
  type Read[+A] = Json[Nothing, A]

  /** A schema that round trips `A`. */
  type Bi[A] = Json[A, A]

  final case class Coerce[-W, +R](self: Annotation[Self.Coerce[Json.Primitive, W, R]]) extends Json[W, R]

  object Coerce
      extends Wrapper.Coerce[Json.Coerce, Json.Primitive](
        [w, r] => (annotation: Annotation[Self.Coerce[Json.Primitive, w, r]]) => new Json.Coerce(annotation),
        [w, r] => (json: Json.Coerce[w, r]) => json.self
      )

  final case class Collection[-W, +R](self: Annotation[Self.Collection[Json, W, R]]) extends Json[W, R]

  object Collection
      extends Wrapper.Collection[Json.Collection, Json](
        [w, r] => (annotation: Annotation[Self.Collection[Json, w, r]]) => new Json.Collection(annotation),
        [w, r] => (json: Json.Collection[w, r]) => json.self
      )

  final case class Constant[-W, +R](self: Annotation[Self.Constant[Json.Primitive, W, R]]) extends Json[W, R]

  object Constant
      extends Wrapper.Constant[Json.Constant, Json.Primitive](
        [w, r] => (annotation: Annotation[Self.Constant[Json.Primitive, w, r]]) => new Json.Constant(annotation),
        [w, r] => (json: Json.Constant[w, r]) => json.self
      )

  final case class Dictionary[-W, +R](self: Annotation[Self.Dictionary[Json, W, R]]) extends Json[W, R]

  object Dictionary
      extends Wrapper.Dictionary[Json.Dictionary, Json](
        [w, r] => (annotation: Annotation[Self.Dictionary[Json, w, r]]) => new Json.Dictionary(annotation),
        [w, r] => (json: Json.Dictionary[w, r]) => json.self
      )

  final case class Optional[-W, +R](self: Annotation[Self.Optional[Json, W, R]]) extends Json[W, R]

  object Optional
      extends Wrapper.Optional[Json.Optional, Json](
        [w, r] => (annotation: Annotation[Self.Optional[Json, w, r]]) => new Json.Optional(annotation),
        [w, r] => (json: Json.Optional[w, r]) => json.self
      )

  final case class Record[-W, +R](self: Annotation[Self.Record[Json.Field, W, R]]) extends Json[W, R]

  object Record
      extends Wrapper.Record[Json.Record, Json.Field](
        [w, r] => (annotation: Annotation[Self.Record[Json.Field, w, r]]) => new Json.Record(annotation),
        [w, r] => (json: Json.Record[w, r]) => json.self
      ):
    given RecordableOperation[Json.Record, Json.Record] = RecordableOperation.identity

    given AppendableOperation[Json.Record, Json.Record, Json.Field] = AppendableOperation.record

  final case class Tuple[-W, +R](self: Annotation[Self.Tuple[Json, W, R]]) extends Json[W, R]

  object Tuple
      extends Wrapper.Tuple[Json.Tuple, Json](
        [w, r] => (annotation: Annotation[Self.Tuple[Json, w, r]]) => new Json.Tuple(annotation),
        [w, r] => (json: Json.Tuple[w, r]) => json.self
      ):
    given TupleableOperation[Json.Tuple, Json.Tuple] = TupleableOperation.identity

    given AppendableOperation[Json.Tuple, Json.Tuple, Json] = AppendableOperation.tuple

  final case class Union[-W, +R](self: Annotation[Self.Union[Json.Branch, W, R]]) extends Json[W, R]

  object Union
      extends Wrapper.Union[Json.Union, Json.Branch](
        [w, r] => (annotation: Annotation[Self.Union[Json.Branch, w, r]]) => new Json.Union(annotation),
        [w, r] => (json: Json.Union[w, r]) => json.self
      ):
    given UnionableOperation[Json.Union, Json.Union] = UnionableOperation.identity

  sealed abstract class Primitive[-W, +R] extends Json[W, R]

  object Primitive:
    final case class Boolean[-W, +R](self: Annotation[Self.Primitive.Boolean[W, R]]) extends Json.Primitive[W, R]

    object Boolean
        extends Wrapper.Primitive.Boolean[Json.Primitive.Boolean](
          [w, r] => (annotation: Annotation[Self.Primitive.Boolean[w, r]]) => new Json.Primitive.Boolean(annotation),
          [w, r] => (json: Json.Primitive.Boolean[w, r]) => json.self
        )

    final case class Number[-W, +R](self: Annotation[Self.Primitive.Number[W, R]]) extends Json.Primitive[W, R]

    object Number
        extends Wrapper.Primitive.Number[Json.Primitive.Number](
          [w, r] => (annotation: Annotation[Self.Primitive.Number[w, r]]) => new Json.Primitive.Number(annotation),
          [w, r] => (json: Json.Primitive.Number[w, r]) => json.self
        )

    final case class Text[-W, +R](self: Annotation[Self.Primitive.Text[W, R]]) extends Json.Primitive[W, R]

    object Text
        extends Wrapper.Primitive.Text[Json.Primitive.Text](
          [w, r] => (annotation: Annotation[Self.Primitive.Text[w, r]]) => new Json.Primitive.Text(annotation),
          [w, r] => (json: Json.Primitive.Text[w, r]) => json.self
        )

    given Profunctor[Json.Primitive]:
      override def dimap[W0, R0, W, R](
          self: Json.Primitive[W0, R0]
      )(f: W => W0)(g: R0 => R): Json.Primitive[W, R] = (self: @unchecked) match
        case self: Json.Primitive.Boolean[W0, R0] => Json.Primitive.Boolean.profunctor.dimap(self)(f)(g)
        case self: Json.Primitive.Number[W0, R0]  => Json.Primitive.Number.profunctor.dimap(self)(f)(g)
        case self: Json.Primitive.Text[W0, R0]    => Json.Primitive.Text.profunctor.dimap(self)(f)(g)

  final case class Field[-W, +R](self: Annotation[Self.Field[Json, W, R]])

  object Field
      extends Wrapper.Field[Json.Field, Json](
        [w, r] => (annotation: Annotation[Self.Field[Json, w, r]]) => new Json.Field(annotation),
        [w, r] => (json: Json.Field[w, r]) => json.self
      ):
    given RecordableOperation[Json.Field, Json.Record] = RecordableOperation.derived

    given AppendableOperation[Json.Field, Json.Record, Json.Field] = AppendableOperation.record

  final case class Branch[-W, +R](self: Annotation[Self.Branch[Json, W, R]])

  object Branch
      extends Wrapper.Branch[Json.Branch, Json](
        [w, r] => (annotation: Annotation[Self.Branch[Json, w, r]]) => new Json.Branch(annotation),
        [w, r] => (json: Json.Branch[w, r]) => json.self
      ):
    given UnionableOperation[Json.Branch, Json.Union] = UnionableOperation.derived

  given Profunctor[Json]:
    override def dimap[W0, R0, W, R](self: Json[W0, R0])(f: W => W0)(g: R0 => R): Json[W, R] =
      (self: @unchecked) match
        case self: Json.Coerce[W0, R0]     => Json.Coerce.profunctor.dimap(self)(f)(g)
        case self: Json.Collection[W0, R0] => Json.Collection.profunctor.dimap(self)(f)(g)
        case self: Json.Constant[W0, R0]   => Json.Constant.profunctor.dimap(self)(f)(g)
        case self: Json.Dictionary[W0, R0] => Json.Dictionary.profunctor.dimap(self)(f)(g)
        case self: Json.Optional[W0, R0]   => Json.Optional.profunctor.dimap(self)(f)(g)
        case self: Json.Primitive[W0, R0]  => Json.Primitive.given_Profunctor_Primitive.dimap(self)(f)(g)
        case self: Json.Record[W0, R0]     => Json.Record.profunctor.dimap(self)(f)(g)
        case self: Json.Tuple[W0, R0]      => Json.Tuple.profunctor.dimap(self)(f)(g)
        case self: Json.Union[W0, R0]      => Json.Union.profunctor.dimap(self)(f)(g)

  given OptionalableOperation[Json, Json.Optional] = OptionalableOperation.derived

  given TupleableOperation[Json, Json.Tuple] = TupleableOperation.derived
