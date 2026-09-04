package io.taig.otter.http

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.arrow.Profunctor
import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Direction
import io.taig.otter.Wrapper

/** A parameter that round trips `A`, whatever it is a parameter of.
  *
  * `Parameter[A]` is what a query parameter or a header holds. A path segment holds a [[Parameter.Value]] instead,
  * which is the same alphabet with repetition taken away.
  */
type Parameter[A] = Parameter.Of[Parameter.Value.Node, A]

object Parameter:
  /** The alphabet of what fits in a parameter.
    *
    * Where a JSON value nests to any depth, a parameter has exactly three levels: the parameter, which may repeat; the
    * value, which is one piece of text; and the primitive that piece of text spells out. Those are the three tiers of
    * this hierarchy, and `S` carries the restriction: a path segment's `S` is bounded to a value, so a repeated segment
    * is a compile error rather than a request no router can match.
    *
    * Most of the format agnostic alphabet is missing here, which is the point. A record, a dictionary and a tuple have
    * no rendering as a parameter, so this alphabet does not name them. Neither does `Optional`: a parameter that may be
    * absent is absent from its *name*, not from its value, and that is [[Self.Field.Optional]] on the [[Query]] or
    * [[Header]] holding it.
    */
  sealed abstract class Schema[+S[-w, +r] <: Parameter.Schema[?, w, r], -W, +R]

  /** A parameter holding `S` and round tripping `A`. */
  type Of[S[-w, +r] <: Parameter.Node[w, r], A] = Parameter.Schema[S, A, A]

  /** The general form, whatever the schema holds. This is what an interpreter that accepts any node is written against.
    */
  type Node = [w, r] =>> Parameter.Schema[?, w, r]

  /** The `S` of a node with nothing inside it. */
  type Leaf = Nothing

  /** The `S` of a node holding both an `S1` and an `S2`, which is what `:*` accumulates over a query string or a header
    * set.
    */
  type Or[S1[-w, +r] <: Parameter.Node[w, r], S2[-w, +r] <: Parameter.Node[w, r]] = [w, r] =>> S1[w, r] | S2[w, r]

  /** A parameter that reads `A`, whatever it writes. */
  type Reader[+A] = Parameter.Reader.Of[Parameter.Value.Node, A]

  object Reader:
    type Of[S[-w, +r] <: Parameter.Node[w, r], +A] = Parameter.Schema[S, Nothing, A]

  /** A parameter that writes `A`, whatever it reads. */
  type Writer[-A] = Parameter.Writer.Of[Parameter.Value.Node, A]

  object Writer:
    type Of[S[-w, +r] <: Parameter.Node[w, r], -A] = Parameter.Schema[S, A, Any]

  type Collection[A] = Parameter.Collection.Of[Parameter.Value.Node, A]

  /** A parameter given more than once.
    *
    * What that looks like on the wire is the position's business, not the value's: a query string repeats the name, a
    * header joins the values with a comma. The schema says only that there are many of them.
    */
  object Collection:
    /** A repetition holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Parameter.Value.Node[w, r], A] = Parameter.Collection.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Parameter.Collection.Schema[Parameter.Value.Node, w, r]

    type Reader[+A] = Parameter.Collection.Reader.Of[Parameter.Value.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Parameter.Value.Node[w, r], +A] = Parameter.Collection.Schema[S, Nothing, A]

    type Writer[-A] = Parameter.Collection.Writer.Of[Parameter.Value.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Parameter.Value.Node[w, r], -A] = Parameter.Collection.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Parameter.Value.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Collection[S, W, R]]
    ) extends Parameter.Schema[S, W, R]

    object Schema
        extends Wrapper.Collection[Parameter.Value.Node, Parameter.Collection.Schema](
          [s[-w, +r] <: Parameter.Value.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Collection[s, w, r]]) => new Parameter.Collection.Schema(annotation),
          [s[-w, +r] <: Parameter.Value.Node[w, r], w, r] =>
            (parameter: Parameter.Collection.Schema[s, w, r]) => parameter.self
        )

  type Value[A] = Parameter.Value.Of[Parameter.Node, A]

  object Value:
    /** A value holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Parameter.Node[w, r], A] = Parameter.Value.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Parameter.Value.Schema[Parameter.Node, w, r]

    type Reader[+A] = Parameter.Value.Reader.Of[Parameter.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Parameter.Node[w, r], +A] = Parameter.Value.Schema[S, Nothing, A]

    type Writer[-A] = Parameter.Value.Writer.Of[Parameter.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Parameter.Node[w, r], -A] = Parameter.Value.Schema[S, A, Any]

    /** Everything that fits in one piece of text.
      *
      * The bound restates its parent's rather than narrowing to `Parameter.Value.Schema`. Narrowing reads as the
      * tighter statement but makes every wildcard a parameter's `S` is compared through recur one generation deeper,
      * and it says nothing the bound on [[Parameter.Collection.Schema]]'s own `S` does not already say.
      */
    sealed abstract class Schema[+S[-w, +r] <: Parameter.Schema[?, w, r], -W, +R] extends Parameter.Schema[S, W, R]

    given profunctor: [S[-w, +r] <: Parameter.Node[w, r]] => Profunctor[[w, r] =>> Parameter.Value.Schema[S, w, r]]:
      override def dimap[W0, R0, W, R](
          self: Parameter.Value.Schema[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Parameter.Value.Schema[S, W, R] = self match
        case self @ Parameter.Coerce.Schema(_)            => Parameter.Coerce.Schema.profunctor.dimap(self)(f)(g)
        case self @ Parameter.Constant.Schema(_)          => Parameter.Constant.Schema.profunctor.dimap(self)(f)(g)
        case self @ Parameter.Enumeration.Schema(_)       => Parameter.Enumeration.Schema.profunctor.dimap(self)(f)(g)
        case self @ Parameter.Primitive.Boolean.Schema(_) =>
          Parameter.Primitive.Boolean.Schema.profunctor.dimap(self)(f)(g)
        case self @ Parameter.Primitive.Number.Schema(_) =>
          Parameter.Primitive.Number.Schema.profunctor.dimap(self)(f)(g)
        case self @ Parameter.Primitive.Text.Schema(_) => Parameter.Primitive.Text.Schema.profunctor.dimap(self)(f)(g)

    given functor: [S[-w, +r] <: Parameter.Node[w, r]] => Functor[[a] =>> Parameter.Value.Schema[S, Nothing, a]] =
      Direction.functor[[w, r] =>> Parameter.Value.Schema[S, w, r]]

    given contravariant: [S[-w, +r] <: Parameter.Node[w, r]]
      => Contravariant[[a] =>> Parameter.Value.Schema[S, a, Any]] =
      Direction.contravariant[[w, r] =>> Parameter.Value.Schema[S, w, r]]

    given invariant: [S[-w, +r] <: Parameter.Node[w, r]] => Invariant[[a] =>> Parameter.Value.Schema[S, a, a]] =
      Direction.invariant[[w, r] =>> Parameter.Value.Schema[S, w, r]]

  type Primitive[A] = Parameter.Primitive.Of[Parameter.Node, A]

  object Primitive:
    /** A primitive holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Parameter.Node[w, r], A] = Parameter.Primitive.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Parameter.Primitive.Schema[Parameter.Node, w, r]

    type Reader[+A] = Parameter.Primitive.Reader.Of[Parameter.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Parameter.Node[w, r], +A] = Parameter.Primitive.Schema[S, Nothing, A]

    type Writer[-A] = Parameter.Primitive.Writer.Of[Parameter.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Parameter.Node[w, r], -A] = Parameter.Primitive.Schema[S, A, Any]

    sealed abstract class Schema[+S[-w, +r] <: Parameter.Schema[?, w, r], -W, +R]
        extends Parameter.Value.Schema[S, W, R]

    type Boolean[A] = Parameter.Primitive.Boolean.Schema[A, A]

    object Boolean:
      /** Holding nothing, so `Node` is the schema itself. */
      type Node = [w, r] =>> Parameter.Primitive.Boolean.Schema[w, r]

      type Reader[+A] = Parameter.Primitive.Boolean.Schema[Nothing, A]

      type Writer[-A] = Parameter.Primitive.Boolean.Schema[A, Any]

      final case class Schema[-W, +R](self: Annotation[Self.Primitive.Boolean[W, R]])
          extends Parameter.Primitive.Schema[Parameter.Leaf, W, R]

      object Schema
          extends Wrapper.Primitive.Boolean[Parameter.Primitive.Boolean.Schema](
            [w, r] =>
              (annotation: Annotation[Self.Primitive.Boolean[w, r]]) =>
                new Parameter.Primitive.Boolean.Schema(annotation),
            [w, r] => (parameter: Parameter.Primitive.Boolean.Schema[w, r]) => parameter.self
          )

    type Number[A] = Parameter.Primitive.Number.Schema[A, A]

    object Number:
      /** Holding nothing, so `Node` is the schema itself. */
      type Node = [w, r] =>> Parameter.Primitive.Number.Schema[w, r]

      type Reader[+A] = Parameter.Primitive.Number.Schema[Nothing, A]

      type Writer[-A] = Parameter.Primitive.Number.Schema[A, Any]

      final case class Schema[-W, +R](self: Annotation[Self.Primitive.Number[W, R]])
          extends Parameter.Primitive.Schema[Parameter.Leaf, W, R]

      object Schema
          extends Wrapper.Primitive.Number[Parameter.Primitive.Number.Schema](
            [w, r] =>
              (annotation: Annotation[Self.Primitive.Number[w, r]]) =>
                new Parameter.Primitive.Number.Schema(annotation),
            [w, r] => (parameter: Parameter.Primitive.Number.Schema[w, r]) => parameter.self
          )

    type Text[A] = Parameter.Primitive.Text.Schema[A, A]

    object Text:
      /** Holding nothing, so `Node` is the schema itself. */
      type Node = [w, r] =>> Parameter.Primitive.Text.Schema[w, r]

      type Reader[+A] = Parameter.Primitive.Text.Schema[Nothing, A]

      type Writer[-A] = Parameter.Primitive.Text.Schema[A, Any]

      final case class Schema[-W, +R](self: Annotation[Self.Primitive.Text[W, R]])
          extends Parameter.Primitive.Schema[Parameter.Leaf, W, R]

      object Schema
          extends Wrapper.Primitive.Text[Parameter.Primitive.Text.Schema](
            [w, r] =>
              (annotation: Annotation[Self.Primitive.Text[w, r]]) => new Parameter.Primitive.Text.Schema(annotation),
            [w, r] => (parameter: Parameter.Primitive.Text.Schema[w, r]) => parameter.self
          )

    given profunctor: [S[-w, +r] <: Parameter.Node[w, r]] => Profunctor[[w, r] =>> Parameter.Primitive.Schema[S, w, r]]:
      override def dimap[W0, R0, W, R](
          self: Parameter.Primitive.Schema[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Parameter.Primitive.Schema[S, W, R] = self match
        case self @ Parameter.Primitive.Boolean.Schema(_) =>
          Parameter.Primitive.Boolean.Schema.profunctor.dimap(self)(f)(g)
        case self @ Parameter.Primitive.Number.Schema(_) =>
          Parameter.Primitive.Number.Schema.profunctor.dimap(self)(f)(g)
        case self @ Parameter.Primitive.Text.Schema(_) => Parameter.Primitive.Text.Schema.profunctor.dimap(self)(f)(g)

    given functor: [S[-w, +r] <: Parameter.Node[w, r]] => Functor[[a] =>> Parameter.Primitive.Schema[S, Nothing, a]] =
      Direction.functor[[w, r] =>> Parameter.Primitive.Schema[S, w, r]]

    given contravariant: [S[-w, +r] <: Parameter.Node[w, r]]
      => Contravariant[[a] =>> Parameter.Primitive.Schema[S, a, Any]] =
      Direction.contravariant[[w, r] =>> Parameter.Primitive.Schema[S, w, r]]

    given invariant: [S[-w, +r] <: Parameter.Node[w, r]] => Invariant[[a] =>> Parameter.Primitive.Schema[S, a, a]] =
      Direction.invariant[[w, r] =>> Parameter.Primitive.Schema[S, w, r]]

  type Coerce[A] = Parameter.Coerce.Of[Parameter.Primitive.Node, A]

  object Coerce:
    /** A coercion holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Parameter.Primitive.Node[w, r], A] = Parameter.Coerce.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Parameter.Coerce.Schema[Parameter.Primitive.Node, w, r]

    type Reader[+A] = Parameter.Coerce.Reader.Of[Parameter.Primitive.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Parameter.Primitive.Node[w, r], +A] = Parameter.Coerce.Schema[S, Nothing, A]

    type Writer[-A] = Parameter.Coerce.Writer.Of[Parameter.Primitive.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Parameter.Primitive.Node[w, r], -A] = Parameter.Coerce.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Parameter.Primitive.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Coerce[S, W, R]]
    ) extends Parameter.Value.Schema[S, W, R]

    object Schema
        extends Wrapper.Coerce[Parameter.Primitive.Node, Parameter.Coerce.Schema](
          [s[-w, +r] <: Parameter.Primitive.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Coerce[s, w, r]]) => new Parameter.Coerce.Schema(annotation),
          [s[-w, +r] <: Parameter.Primitive.Node[w, r], w, r] =>
            (parameter: Parameter.Coerce.Schema[s, w, r]) => parameter.self
        )

  type Constant[A] = Parameter.Constant.Of[Parameter.Primitive.Node, A]

  object Constant:
    /** A constant holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Parameter.Primitive.Node[w, r], A] = Parameter.Constant.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Parameter.Constant.Schema[Parameter.Primitive.Node, w, r]

    type Reader[+A] = Parameter.Constant.Reader.Of[Parameter.Primitive.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Parameter.Primitive.Node[w, r], +A] = Parameter.Constant.Schema[S, Nothing, A]

    type Writer[-A] = Parameter.Constant.Writer.Of[Parameter.Primitive.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Parameter.Primitive.Node[w, r], -A] = Parameter.Constant.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Parameter.Primitive.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Constant[S, W, R]]
    ) extends Parameter.Value.Schema[S, W, R]

    object Schema
        extends Wrapper.Constant[Parameter.Primitive.Node, Parameter.Constant.Schema](
          [s[-w, +r] <: Parameter.Primitive.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Constant[s, w, r]]) => new Parameter.Constant.Schema(annotation),
          [s[-w, +r] <: Parameter.Primitive.Node[w, r], w, r] =>
            (parameter: Parameter.Constant.Schema[s, w, r]) => parameter.self
        )

  type Enumeration[A] = Parameter.Enumeration.Of[Parameter.Primitive.Node, A]

  object Enumeration:
    /** An enumeration holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Parameter.Primitive.Node[w, r], A] = Parameter.Enumeration.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Parameter.Enumeration.Schema[Parameter.Primitive.Node, w, r]

    type Reader[+A] = Parameter.Enumeration.Reader.Of[Parameter.Primitive.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Parameter.Primitive.Node[w, r], +A] = Parameter.Enumeration.Schema[S, Nothing, A]

    type Writer[-A] = Parameter.Enumeration.Writer.Of[Parameter.Primitive.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Parameter.Primitive.Node[w, r], -A] = Parameter.Enumeration.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Parameter.Primitive.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Enumeration[S, W, R]]
    ) extends Parameter.Value.Schema[S, W, R]

    object Schema
        extends Wrapper.Enumeration[Parameter.Primitive.Node, Parameter.Enumeration.Schema](
          [s[-w, +r] <: Parameter.Primitive.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Enumeration[s, w, r]]) => new Parameter.Enumeration.Schema(annotation),
          [s[-w, +r] <: Parameter.Primitive.Node[w, r], w, r] =>
            (parameter: Parameter.Enumeration.Schema[s, w, r]) => parameter.self
        )

  given profunctor: [S[-w, +r] <: Parameter.Node[w, r]] => Profunctor[[w, r] =>> Parameter.Schema[S, w, r]]:
    override def dimap[W0, R0, W, R](
        self: Parameter.Schema[S, W0, R0]
    )(f: W => W0)(g: R0 => R): Parameter.Schema[S, W, R] = self match
      case self @ Parameter.Collection.Schema(_)        => Parameter.Collection.Schema.profunctor.dimap(self)(f)(g)
      case self @ Parameter.Coerce.Schema(_)            => Parameter.Coerce.Schema.profunctor.dimap(self)(f)(g)
      case self @ Parameter.Constant.Schema(_)          => Parameter.Constant.Schema.profunctor.dimap(self)(f)(g)
      case self @ Parameter.Enumeration.Schema(_)       => Parameter.Enumeration.Schema.profunctor.dimap(self)(f)(g)
      case self @ Parameter.Primitive.Boolean.Schema(_) =>
        Parameter.Primitive.Boolean.Schema.profunctor.dimap(self)(f)(g)
      case self @ Parameter.Primitive.Number.Schema(_) => Parameter.Primitive.Number.Schema.profunctor.dimap(self)(f)(g)
      case self @ Parameter.Primitive.Text.Schema(_)   => Parameter.Primitive.Text.Schema.profunctor.dimap(self)(f)(g)

  given functor: [S[-w, +r] <: Parameter.Node[w, r]] => Functor[[a] =>> Parameter.Schema[S, Nothing, a]] =
    Direction.functor[[w, r] =>> Parameter.Schema[S, w, r]]

  given contravariant: [S[-w, +r] <: Parameter.Node[w, r]] => Contravariant[[a] =>> Parameter.Schema[S, a, Any]] =
    Direction.contravariant[[w, r] =>> Parameter.Schema[S, w, r]]

  given invariant: [S[-w, +r] <: Parameter.Node[w, r]] => Invariant[[a] =>> Parameter.Schema[S, a, a]] =
    Direction.invariant[[w, r] =>> Parameter.Schema[S, w, r]]
