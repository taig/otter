package io.taig.otter

import cats.arrow.Profunctor
import io.taig.otter as Self
import io.taig.otter.operation.*

/** A JSON schema that round trips `A`.
  *
  * Every node comes in four forms: `Json.Record[A]` round trips, `Json.Record.Reader[A]` reads, `Json.Record.Writer[A]`
  * writes, and `Json.Record.Of[+S, -W, +R]` is the general form the other three abbreviate. A round tripping schema is
  * both a reader and a writer.
  */
type Json[A] = Json.Node[A, A]

object Json:
  /** The JSON schema alphabet.
    *
    * Each member wraps one format agnostic node in an [[Annotation]] and ties the recursive knot by referring back to
    * `Json.Of`. `W` is the type this schema writes, `R` the type it reads, and `S` the type of what is inside it: the
    * element of a collection, the fields of a record, the branches of a union.
    *
    * `S` is what makes a restricted schema expressible. `Json.Record.Of[Json.Primitive.Of, A, A]` is a record whose
    * fields are all primitives, so a conversion into a flat format can demand exactly that and reject anything nested.
    * It is not a witness the DSL maintains by hand: a field of type `Json.Field.Of[S, W, R]` genuinely holds an `S`.
    */
  sealed abstract class Of[+S[-_, +_], -W, +R]

  /** The general form, whatever the schema holds. This is what an interpreter that accepts any node is written against:
    * `Decoder[Json.Node, CirceJson]`.
    */
  type Node = [w, r] =>> Json.Of[?, w, r]

  /** The `S` of a node with nothing inside it. It is the bottom of the constructor lattice, so a leaf widens to fit
    * wherever a child is expected, exactly as `Record.Empty` does in the format agnostic layer.
    */
  type Leaf = Nothing

  /** The `S` of a node holding both an `S1` and an `S2`, which is what `:*` and `:+` accumulate. */
  type Or[S1[-_, +_], S2[-_, +_]] = [w, r] =>> S1[w, r] | S2[w, r]

  /** A schema that reads `A`, whatever it writes.
    *
    * Every node carries the same pair. `Json.Record[A] <: Json.Record.Reader[A]`, so a round tripping schema is
    * accepted wherever a reader is asked for.
    */
  type Reader[+A] = Json.Of[Json.Node, Nothing, A]

  /** A schema that writes `A`, whatever it reads. */
  type Writer[-A] = Json.Of[Json.Node, A, Any]

  type Coerce[A] = Json.Coerce.Of[Json.Primitive.Node, A, A]

  object Coerce:
    type Reader[+A] = Json.Coerce.Of[Json.Primitive.Node, Nothing, A]

    type Writer[-A] = Json.Coerce.Of[Json.Primitive.Node, A, Any]

    type Node = [w, r] =>> Json.Coerce.Of[Json.Primitive.Node, w, r]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Coerce[S, W, R]]) extends Json.Of[S, W, R]

    object Of
        extends Wrapper.Coerce[Json.Coerce.Of](
          [s[-_, +_], w, r] => (annotation: Annotation[Self.Coerce[s, w, r]]) => new Json.Coerce.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Coerce.Of[s, w, r]) => json.self
        )

  type Collection[A] = Json.Collection.Of[Json.Node, A, A]

  object Collection:
    type Reader[+A] = Json.Collection.Of[Json.Node, Nothing, A]

    type Writer[-A] = Json.Collection.Of[Json.Node, A, Any]

    type Node = [w, r] =>> Json.Collection.Of[Json.Node, w, r]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Collection[S, W, R]]) extends Json.Of[S, W, R]

    object Of
        extends Wrapper.Collection[Json.Collection.Of](
          [s[-_, +_], w, r] => (annotation: Annotation[Self.Collection[s, w, r]]) => new Json.Collection.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Collection.Of[s, w, r]) => json.self
        )

  type Constant[A] = Json.Constant.Of[Json.Primitive.Node, A, A]

  object Constant:
    type Reader[+A] = Json.Constant.Of[Json.Primitive.Node, Nothing, A]

    type Writer[-A] = Json.Constant.Of[Json.Primitive.Node, A, Any]

    type Node = [w, r] =>> Json.Constant.Of[Json.Primitive.Node, w, r]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Constant[S, W, R]]) extends Json.Of[S, W, R]

    object Of
        extends Wrapper.Constant[Json.Constant.Of](
          [s[-_, +_], w, r] => (annotation: Annotation[Self.Constant[s, w, r]]) => new Json.Constant.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Constant.Of[s, w, r]) => json.self
        )

  type Dictionary[A] = Json.Dictionary.Of[Json.Node, A, A]

  object Dictionary:
    type Reader[+A] = Json.Dictionary.Of[Json.Node, Nothing, A]

    type Writer[-A] = Json.Dictionary.Of[Json.Node, A, Any]

    type Node = [w, r] =>> Json.Dictionary.Of[Json.Node, w, r]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Dictionary[S, W, R]]) extends Json.Of[S, W, R]

    object Of
        extends Wrapper.Dictionary[Json.Dictionary.Of](
          [s[-_, +_], w, r] => (annotation: Annotation[Self.Dictionary[s, w, r]]) => new Json.Dictionary.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Dictionary.Of[s, w, r]) => json.self
        )

  type Enumeration[A] = Json.Enumeration.Of[Json.Primitive.Node, A, A]

  object Enumeration:
    type Reader[+A] = Json.Enumeration.Of[Json.Primitive.Node, Nothing, A]

    type Writer[-A] = Json.Enumeration.Of[Json.Primitive.Node, A, Any]

    type Node = [w, r] =>> Json.Enumeration.Of[Json.Primitive.Node, w, r]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Enumeration[S, W, R]]) extends Json.Of[S, W, R]

    object Of
        extends Wrapper.Enumeration[Json.Enumeration.Of](
          [s[-_, +_], w, r] =>
            (annotation: Annotation[Self.Enumeration[s, w, r]]) => new Json.Enumeration.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Enumeration.Of[s, w, r]) => json.self
        )

  type Optional[A] = Json.Optional.Of[Json.Node, A, A]

  object Optional:
    type Reader[+A] = Json.Optional.Of[Json.Node, Nothing, A]

    type Writer[-A] = Json.Optional.Of[Json.Node, A, Any]

    type Node = [w, r] =>> Json.Optional.Of[Json.Node, w, r]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Optional[S, W, R]]) extends Json.Of[S, W, R]

    object Of
        extends Wrapper.Optional[Json.Optional.Of](
          [s[-_, +_], w, r] => (annotation: Annotation[Self.Optional[s, w, r]]) => new Json.Optional.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Optional.Of[s, w, r]) => json.self
        )

  type Record[A] = Json.Record.Of[Json.Node, A, A]

  object Record:
    type Reader[+A] = Json.Record.Of[Json.Node, Nothing, A]

    type Writer[-A] = Json.Record.Of[Json.Node, A, Any]

    type Node = [w, r] =>> Json.Record.Of[Json.Node, w, r]

    /** A record whose fields are all primitives, which is what a flat format can represent. */
    type Flat[A] = Json.Record.Of[Json.Primitive.Node, A, A]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Record[[w, r] =>> Json.Field.Of[S, w, r], W, R]])
        extends Json.Of[S, W, R]

    object Of
        extends Wrapper.Record[Json.Record.Of, Json.Field.Of](
          [s[-_, +_], w, r] =>
            (annotation: Annotation[Self.Record[[a, b] =>> Json.Field.Of[s, a, b], w, r]]) =>
              new Json.Record.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Record.Of[s, w, r]) => json.self
        ):
      given recordable: [S[-_, +_]]
        => RecordableOperation[[w, r] =>> Json.Record.Of[S, w, r], [w, r] =>> Json.Record.Of[S, w, r]] =
        RecordableOperation.identity

      /** `record :* field`. The result carries both children's `S`, so the union accumulates down the chain. */
      given appendable: [S1[-_, +_], S2[-_, +_]]
          => AppendableOperation[
            [w, r] =>> Json.Record.Of[S1, w, r],
            [w, r] =>> Json.Record.Of[Json.Or[S1, S2], w, r],
            [w, r] =>> Json.Field.Of[S2, w, r]
          ]:
        override def lift[W, R](fa: Json.Record.Of[S1, W, R]): Json.Record.Of[Json.Or[S1, S2], W, R] = fa

        override def element[W, R](fb: => Json.Field.Of[S2, W, R]): Json.Record.Of[Json.Or[S1, S2], W, R] =
          Json.Record.Of.apply[Json.Or[S1, S2], W, R](Self.Record.Root(Reference.later(fb)))

  type Tuple[A] = Json.Tuple.Of[Json.Node, A, A]

  object Tuple:
    type Reader[+A] = Json.Tuple.Of[Json.Node, Nothing, A]

    type Writer[-A] = Json.Tuple.Of[Json.Node, A, Any]

    type Node = [w, r] =>> Json.Tuple.Of[Json.Node, w, r]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Tuple[S, W, R]]) extends Json.Of[S, W, R]

    object Of
        extends Wrapper.Tuple[Json.Tuple.Of](
          [s[-_, +_], w, r] => (annotation: Annotation[Self.Tuple[s, w, r]]) => new Json.Tuple.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Tuple.Of[s, w, r]) => json.self
        ):
      given tupleable: [S[-_, +_]]
        => TupleableOperation[[w, r] =>> Json.Tuple.Of[S, w, r], [w, r] =>> Json.Tuple.Of[S, w, r]] =
        TupleableOperation.identity

      /** `tuple :* schema`. */
      given appendable: [S1[-_, +_], S2[-_, +_]]
          => AppendableOperation[
            [w, r] =>> Json.Tuple.Of[S1, w, r],
            [w, r] =>> Json.Tuple.Of[Json.Or[S1, S2], w, r],
            S2
          ]:
        override def lift[W, R](fa: Json.Tuple.Of[S1, W, R]): Json.Tuple.Of[Json.Or[S1, S2], W, R] = fa

        override def element[W, R](fb: => S2[W, R]): Json.Tuple.Of[Json.Or[S1, S2], W, R] =
          Json.Tuple.Of.apply[Json.Or[S1, S2], W, R](Self.Tuple.Root(Reference.later(fb)))

  type Union[A] = Json.Union.Of[Json.Node, A, A]

  object Union:
    type Reader[+A] = Json.Union.Of[Json.Node, Nothing, A]

    type Writer[-A] = Json.Union.Of[Json.Node, A, Any]

    type Node = [w, r] =>> Json.Union.Of[Json.Node, w, r]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Union[[w, r] =>> Json.Branch.Of[S, w, r], W, R]])
        extends Json.Of[S, W, R]

    object Of
        extends Wrapper.Union[Json.Union.Of, Json.Branch.Of](
          [s[-_, +_], w, r] =>
            (annotation: Annotation[Self.Union[[a, b] =>> Json.Branch.Of[s, a, b], w, r]]) =>
              new Json.Union.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Union.Of[s, w, r]) => json.self
        ):
      given unionable: [S[-_, +_]]
        => UnionableOperation[[w, r] =>> Json.Union.Of[S, w, r], [w, r] =>> Json.Union.Of[S, w, r]] =
        UnionableOperation.identity

      /** `union :+ branch`. */
      given alternable: [S1[-_, +_], S2[-_, +_]]
          => AlternableOperation[
            [w, r] =>> Json.Union.Of[S1, w, r],
            [w, r] =>> Json.Union.Of[Json.Or[S1, S2], w, r],
            [w, r] =>> Json.Branch.Of[S2, w, r]
          ]:
        override def lift[W, R](fa: Json.Union.Of[S1, W, R]): Json.Union.Of[Json.Or[S1, S2], W, R] = fa

        override def element[W, R](fb: => Json.Branch.Of[S2, W, R]): Json.Union.Of[Json.Or[S1, S2], W, R] =
          Json.Union.Of.apply[Json.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))

  type Primitive[A] = Json.Primitive.Of[Json.Node, A, A]

  object Primitive:
    type Reader[+A] = Json.Primitive.Of[Json.Node, Nothing, A]

    type Writer[-A] = Json.Primitive.Of[Json.Node, A, Any]

    type Node = [w, r] =>> Json.Primitive.Of[Json.Node, w, r]

    sealed abstract class Of[+S[-_, +_], -W, +R] extends Json.Of[S, W, R]

    type Boolean[A] = Json.Primitive.Boolean.Of[A, A]

    object Boolean:
      type Reader[+A] = Json.Primitive.Boolean.Of[Nothing, A]

      type Writer[-A] = Json.Primitive.Boolean.Of[A, Any]

      final case class Of[-W, +R](self: Annotation[Self.Primitive.Boolean[W, R]])
          extends Json.Primitive.Of[Json.Leaf, W, R]

      object Of
          extends Wrapper.Primitive.Boolean[Json.Primitive.Boolean.Of](
            [w, r] =>
              (annotation: Annotation[Self.Primitive.Boolean[w, r]]) => new Json.Primitive.Boolean.Of(annotation),
            [w, r] => (json: Json.Primitive.Boolean.Of[w, r]) => json.self
          )

    type Number[A] = Json.Primitive.Number.Of[A, A]

    object Number:
      type Reader[+A] = Json.Primitive.Number.Of[Nothing, A]

      type Writer[-A] = Json.Primitive.Number.Of[A, Any]

      final case class Of[-W, +R](self: Annotation[Self.Primitive.Number[W, R]])
          extends Json.Primitive.Of[Json.Leaf, W, R]

      object Of
          extends Wrapper.Primitive.Number[Json.Primitive.Number.Of](
            [w, r] => (annotation: Annotation[Self.Primitive.Number[w, r]]) => new Json.Primitive.Number.Of(annotation),
            [w, r] => (json: Json.Primitive.Number.Of[w, r]) => json.self
          )

    type Text[A] = Json.Primitive.Text.Of[A, A]

    object Text:
      type Reader[+A] = Json.Primitive.Text.Of[Nothing, A]

      type Writer[-A] = Json.Primitive.Text.Of[A, Any]

      final case class Of[-W, +R](self: Annotation[Self.Primitive.Text[W, R]])
          extends Json.Primitive.Of[Json.Leaf, W, R]

      object Of
          extends Wrapper.Primitive.Text[Json.Primitive.Text.Of](
            [w, r] => (annotation: Annotation[Self.Primitive.Text[w, r]]) => new Json.Primitive.Text.Of(annotation),
            [w, r] => (json: Json.Primitive.Text.Of[w, r]) => json.self
          )

    given profunctor: [S[-_, +_]] => Profunctor[[w, r] =>> Json.Primitive.Of[S, w, r]]:
      override def dimap[W0, R0, W, R](
          self: Json.Primitive.Of[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Json.Primitive.Of[S, W, R] = (self: @unchecked) match
        case self: Json.Primitive.Boolean.Of[W0, R0] => Json.Primitive.Boolean.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Primitive.Number.Of[W0, R0]  => Json.Primitive.Number.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Primitive.Text.Of[W0, R0]    => Json.Primitive.Text.Of.profunctor.dimap(self)(f)(g)

  type Field[A] = Json.Field.Of[Json.Node, A, A]

  object Field:
    type Reader[+A] = Json.Field.Of[Json.Node, Nothing, A]

    type Writer[-A] = Json.Field.Of[Json.Node, A, Any]

    type Node = [w, r] =>> Json.Field.Of[Json.Node, w, r]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Field[S, W, R]])

    object Of
        extends Wrapper.Field[Json.Field.Of](
          [s[-_, +_], w, r] => (annotation: Annotation[Self.Field[s, w, r]]) => new Json.Field.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Field.Of[s, w, r]) => json.self
        ):
      given recordable: [S[-_, +_]]
        => RecordableOperation[[w, r] =>> Json.Field.Of[S, w, r], [w, r] =>> Json.Record.Of[S, w, r]] =
        RecordableOperation.derived

      /** `field :* field`. */
      given appendable: [S1[-_, +_], S2[-_, +_]]
          => AppendableOperation[
            [w, r] =>> Json.Field.Of[S1, w, r],
            [w, r] =>> Json.Record.Of[Json.Or[S1, S2], w, r],
            [w, r] =>> Json.Field.Of[S2, w, r]
          ]:
        override def lift[W, R](fa: Json.Field.Of[S1, W, R]): Json.Record.Of[Json.Or[S1, S2], W, R] =
          Json.Record.Of.apply[Json.Or[S1, S2], W, R](Self.Record.Root(Reference.now(fa)))

        override def element[W, R](fb: => Json.Field.Of[S2, W, R]): Json.Record.Of[Json.Or[S1, S2], W, R] =
          Json.Record.Of.apply[Json.Or[S1, S2], W, R](Self.Record.Root(Reference.later(fb)))

  type Branch[A] = Json.Branch.Of[Json.Node, A, A]

  object Branch:
    type Reader[+A] = Json.Branch.Of[Json.Node, Nothing, A]

    type Writer[-A] = Json.Branch.Of[Json.Node, A, Any]

    type Node = [w, r] =>> Json.Branch.Of[Json.Node, w, r]

    final case class Of[+S[-_, +_], -W, +R](self: Annotation[Self.Branch[S, W, R]])

    object Of
        extends Wrapper.Branch[Json.Branch.Of](
          [s[-_, +_], w, r] => (annotation: Annotation[Self.Branch[s, w, r]]) => new Json.Branch.Of(annotation),
          [s[-_, +_], w, r] => (json: Json.Branch.Of[s, w, r]) => json.self
        ):
      given unionable: [S[-_, +_]]
        => UnionableOperation[[w, r] =>> Json.Branch.Of[S, w, r], [w, r] =>> Json.Union.Of[S, w, r]] =
        UnionableOperation.derived

      /** `branch :+ branch`. */
      given alternable: [S1[-_, +_], S2[-_, +_]]
          => AlternableOperation[
            [w, r] =>> Json.Branch.Of[S1, w, r],
            [w, r] =>> Json.Union.Of[Json.Or[S1, S2], w, r],
            [w, r] =>> Json.Branch.Of[S2, w, r]
          ]:
        override def lift[W, R](fa: Json.Branch.Of[S1, W, R]): Json.Union.Of[Json.Or[S1, S2], W, R] =
          Json.Union.Of.apply[Json.Or[S1, S2], W, R](Self.Union.Root(Reference.now(fa)))

        override def element[W, R](fb: => Json.Branch.Of[S2, W, R]): Json.Union.Of[Json.Or[S1, S2], W, R] =
          Json.Union.Of.apply[Json.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))

  given profunctor: [S[-_, +_]] => Profunctor[[w, r] =>> Json.Of[S, w, r]]:
    override def dimap[W0, R0, W, R](self: Json.Of[S, W0, R0])(f: W => W0)(g: R0 => R): Json.Of[S, W, R] =
      (self: @unchecked) match
        case self: Json.Coerce.Of[S, W0, R0]      => Json.Coerce.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Collection.Of[S, W0, R0]  => Json.Collection.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Constant.Of[S, W0, R0]    => Json.Constant.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Dictionary.Of[S, W0, R0]  => Json.Dictionary.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Enumeration.Of[S, W0, R0] => Json.Enumeration.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Optional.Of[S, W0, R0]    => Json.Optional.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Primitive.Of[S, W0, R0]   => Json.Primitive.profunctor.dimap(self)(f)(g)
        case self: Json.Record.Of[S, W0, R0]      => Json.Record.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Tuple.Of[S, W0, R0]       => Json.Tuple.Of.profunctor.dimap(self)(f)(g)
        case self: Json.Union.Of[S, W0, R0]       => Json.Union.Of.profunctor.dimap(self)(f)(g)

  /** `S` is bounded to a schema so that these do not also offer `.optional` and `.toTuple` on a field or a branch,
    * which are not schemas and already carry their own `optional`.
    */
  given optionalable: [S[-w, +r] <: Json.Node[w, r]]
    => OptionalableOperation[S, [w, r] =>> Json.Optional.Of[S, w, r]] = OptionalableOperation.derived

  given tupleable: [S[-w, +r] <: Json.Node[w, r]] => TupleableOperation[S, [w, r] =>> Json.Tuple.Of[S, w, r]] =
    TupleableOperation.derived
