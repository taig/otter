package io.taig.otter

import cats.arrow.Profunctor
import io.taig.otter as Self
import io.taig.otter.operation.*

/** A JSON schema that round trips `A`. Every node also has a two parameter general form, `Json.Record.Of[-W, +R]` and
  * so on, for the schemas that only read or only write.
  */
type Json[A] = Json.Of[A, A]

object Json:
  /** The JSON schema alphabet.
    *
    * Each member wraps one format agnostic node in an [[Annotation]] and ties the recursive knot by referring back to
    * `Json.Of`. `W` is the type this schema writes, `R` the type it reads.
    */
  sealed abstract class Of[-W, +R]

  /** A schema that at least writes `A`. */
  type Write[-A] = Json.Of[A, Any]

  /** A schema that at least reads `A`. */
  type Read[+A] = Json.Of[Nothing, A]

  type Coerce[A] = Json.Coerce.Of[A, A]

  object Coerce:
    final case class Of[-W, +R](self: Annotation[Self.Coerce[Json.Primitive.Of, W, R]]) extends Json.Of[W, R]

    object Of
        extends Wrapper.Coerce[Json.Coerce.Of, Json.Primitive.Of](
          [w, r] => (annotation: Annotation[Self.Coerce[Json.Primitive.Of, w, r]]) => new Json.Coerce.Of(annotation),
          [w, r] => (json: Json.Coerce.Of[w, r]) => json.self
        )

  type Collection[A] = Json.Collection.Of[A, A]

  object Collection:
    final case class Of[-W, +R](self: Annotation[Self.Collection[Json.Of, W, R]]) extends Json.Of[W, R]

    object Of
        extends Wrapper.Collection[Json.Collection.Of, Json.Of](
          [w, r] => (annotation: Annotation[Self.Collection[Json.Of, w, r]]) => new Json.Collection.Of(annotation),
          [w, r] => (json: Json.Collection.Of[w, r]) => json.self
        )

  type Constant[A] = Json.Constant.Of[A, A]

  object Constant:
    final case class Of[-W, +R](self: Annotation[Self.Constant[Json.Primitive.Of, W, R]]) extends Json.Of[W, R]

    object Of
        extends Wrapper.Constant[Json.Constant.Of, Json.Primitive.Of](
          [w, r] =>
            (annotation: Annotation[Self.Constant[Json.Primitive.Of, w, r]]) => new Json.Constant.Of(annotation),
          [w, r] => (json: Json.Constant.Of[w, r]) => json.self
        )

  type Dictionary[A] = Json.Dictionary.Of[A, A]

  object Dictionary:
    final case class Of[-W, +R](self: Annotation[Self.Dictionary[Json.Of, W, R]]) extends Json.Of[W, R]

    object Of
        extends Wrapper.Dictionary[Json.Dictionary.Of, Json.Of](
          [w, r] => (annotation: Annotation[Self.Dictionary[Json.Of, w, r]]) => new Json.Dictionary.Of(annotation),
          [w, r] => (json: Json.Dictionary.Of[w, r]) => json.self
        )

  type Enumeration[A] = Json.Enumeration.Of[A, A]

  object Enumeration:
    final case class Of[-W, +R](self: Annotation[Self.Enumeration[Json.Primitive.Of, W, R]]) extends Json.Of[W, R]

    object Of
        extends Wrapper.Enumeration[Json.Enumeration.Of, Json.Primitive.Of](
          [w, r] =>
            (annotation: Annotation[Self.Enumeration[Json.Primitive.Of, w, r]]) => new Json.Enumeration.Of(annotation),
          [w, r] => (json: Json.Enumeration.Of[w, r]) => json.self
        )

  type Optional[A] = Json.Optional.Of[A, A]

  object Optional:
    final case class Of[-W, +R](self: Annotation[Self.Optional[Json.Of, W, R]]) extends Json.Of[W, R]

    object Of
        extends Wrapper.Optional[Json.Optional.Of, Json.Of](
          [w, r] => (annotation: Annotation[Self.Optional[Json.Of, w, r]]) => new Json.Optional.Of(annotation),
          [w, r] => (json: Json.Optional.Of[w, r]) => json.self
        )

  type Record[A] = Json.Record.Of[A, A]

  object Record:
    final case class Of[-W, +R](self: Annotation[Self.Record[Json.Field.Of, W, R]]) extends Json.Of[W, R]

    object Of
        extends Wrapper.Record[Json.Record.Of, Json.Field.Of](
          [w, r] => (annotation: Annotation[Self.Record[Json.Field.Of, w, r]]) => new Json.Record.Of(annotation),
          [w, r] => (json: Json.Record.Of[w, r]) => json.self
        ):
      given RecordableOperation[Json.Record.Of, Json.Record.Of] = RecordableOperation.identity

      given AppendableOperation[Json.Record.Of, Json.Record.Of, Json.Field.Of] = AppendableOperation.record

  type Tuple[A] = Json.Tuple.Of[A, A]

  object Tuple:
    final case class Of[-W, +R](self: Annotation[Self.Tuple[Json.Of, W, R]]) extends Json.Of[W, R]

    object Of
        extends Wrapper.Tuple[Json.Tuple.Of, Json.Of](
          [w, r] => (annotation: Annotation[Self.Tuple[Json.Of, w, r]]) => new Json.Tuple.Of(annotation),
          [w, r] => (json: Json.Tuple.Of[w, r]) => json.self
        ):
      given TupleableOperation[Json.Tuple.Of, Json.Tuple.Of] = TupleableOperation.identity

      given AppendableOperation[Json.Tuple.Of, Json.Tuple.Of, Json.Of] = AppendableOperation.tuple

  type Union[A] = Json.Union.Of[A, A]

  object Union:
    final case class Of[-W, +R](self: Annotation[Self.Union[Json.Branch.Of, W, R]]) extends Json.Of[W, R]

    object Of
        extends Wrapper.Union[Json.Union.Of, Json.Branch.Of](
          [w, r] => (annotation: Annotation[Self.Union[Json.Branch.Of, w, r]]) => new Json.Union.Of(annotation),
          [w, r] => (json: Json.Union.Of[w, r]) => json.self
        ):
      given UnionableOperation[Json.Union.Of, Json.Union.Of] = UnionableOperation.identity

  type Primitive[A] = Json.Primitive.Of[A, A]

  object Primitive:
    sealed abstract class Of[-W, +R] extends Json.Of[W, R]

    type Boolean[A] = Json.Primitive.Boolean.Of[A, A]

    object Boolean:
      final case class Of[-W, +R](self: Annotation[Self.Primitive.Boolean[W, R]]) extends Json.Primitive.Of[W, R]

      object Of
          extends Wrapper.Primitive.Boolean[Json.Primitive.Boolean.Of](
            [w, r] =>
              (annotation: Annotation[Self.Primitive.Boolean[w, r]]) => new Json.Primitive.Boolean.Of(annotation),
            [w, r] => (json: Json.Primitive.Boolean.Of[w, r]) => json.self
          )

    type Number[A] = Json.Primitive.Number.Of[A, A]

    object Number:
      final case class Of[-W, +R](self: Annotation[Self.Primitive.Number[W, R]]) extends Json.Primitive.Of[W, R]

      object Of
          extends Wrapper.Primitive.Number[Json.Primitive.Number.Of](
            [w, r] => (annotation: Annotation[Self.Primitive.Number[w, r]]) => new Json.Primitive.Number.Of(annotation),
            [w, r] => (json: Json.Primitive.Number.Of[w, r]) => json.self
          )

    type Text[A] = Json.Primitive.Text.Of[A, A]

    object Text:
      final case class Of[-W, +R](self: Annotation[Self.Primitive.Text[W, R]]) extends Json.Primitive.Of[W, R]

      object Of
          extends Wrapper.Primitive.Text[Json.Primitive.Text.Of](
            [w, r] => (annotation: Annotation[Self.Primitive.Text[w, r]]) => new Json.Primitive.Text.Of(annotation),
            [w, r] => (json: Json.Primitive.Text.Of[w, r]) => json.self
          )

    given profunctor: Profunctor[Json.Primitive.Of]:
      override def dimap[W0, R0, W, R](
          self: Json.Primitive.Of[W0, R0]
      )(f: W => W0)(g: R0 => R): Json.Primitive.Of[W, R] = (self: @unchecked) match
        case self: Json.Primitive.Boolean.Of[W0, R0] => Json.Primitive.Boolean.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Primitive.Number.Of[W0, R0]  => Json.Primitive.Number.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Primitive.Text.Of[W0, R0]    => Json.Primitive.Text.Of.profunctor.dimap(self)(f)(g)

  type Field[A] = Json.Field.Of[A, A]

  object Field:
    final case class Of[-W, +R](self: Annotation[Self.Field[Json.Of, W, R]])

    object Of
        extends Wrapper.Field[Json.Field.Of, Json.Of](
          [w, r] => (annotation: Annotation[Self.Field[Json.Of, w, r]]) => new Json.Field.Of(annotation),
          [w, r] => (json: Json.Field.Of[w, r]) => json.self
        ):
      given RecordableOperation[Json.Field.Of, Json.Record.Of] = RecordableOperation.derived

      given AppendableOperation[Json.Field.Of, Json.Record.Of, Json.Field.Of] = AppendableOperation.record

  type Branch[A] = Json.Branch.Of[A, A]

  object Branch:
    final case class Of[-W, +R](self: Annotation[Self.Branch[Json.Of, W, R]])

    object Of
        extends Wrapper.Branch[Json.Branch.Of, Json.Of](
          [w, r] => (annotation: Annotation[Self.Branch[Json.Of, w, r]]) => new Json.Branch.Of(annotation),
          [w, r] => (json: Json.Branch.Of[w, r]) => json.self
        ):
      given UnionableOperation[Json.Branch.Of, Json.Union.Of] = UnionableOperation.derived

  given profunctor: Profunctor[Json.Of]:
    override def dimap[W0, R0, W, R](self: Json.Of[W0, R0])(f: W => W0)(g: R0 => R): Json.Of[W, R] =
      (self: @unchecked) match
        case self: Json.Coerce.Of[W0, R0]      => Json.Coerce.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Collection.Of[W0, R0]  => Json.Collection.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Constant.Of[W0, R0]    => Json.Constant.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Dictionary.Of[W0, R0]  => Json.Dictionary.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Enumeration.Of[W0, R0] => Json.Enumeration.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Optional.Of[W0, R0]    => Json.Optional.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Primitive.Of[W0, R0]   => Json.Primitive.profunctor.dimap(self)(f)(g)
        case self: Json.Record.Of[W0, R0]      => Json.Record.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Tuple.Of[W0, R0]       => Json.Tuple.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Union.Of[W0, R0]       => Json.Union.Of.profunctor.dimap(self)(f)(g)

  given OptionalableOperation[Json.Of, Json.Optional.Of] = OptionalableOperation.derived

  given TupleableOperation[Json.Of, Json.Tuple.Of] = TupleableOperation.derived
