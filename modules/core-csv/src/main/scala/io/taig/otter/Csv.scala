package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.arrow.Profunctor
import io.taig.otter as Self
import io.taig.otter.operation.*

/** A CSV schema that round trips `A`.
  *
  * A CSV row is the entry point rather than an arbitrary schema, so `Csv[A]` is a record whose members are all cells.
  * `Csv.Tuple[A]` is the same row without a header, addressed by position.
  */
type Csv[A] = Csv.Record.Of[Csv.Cell.Node, A]

object Csv:
  /** The CSV schema alphabet.
    *
    * Where a JSON value nests to any depth, a CSV file has exactly three levels: the row, the cell it is made of, and
    * the primitive the cell spells out. Those are the three tiers of this hierarchy, and `S` is what carries the
    * restriction: a row's `S` is bounded to a cell, so `Csv.Field.Schema` cannot hold another row and the format's
    * flatness is a compile error rather than a runtime failure.
    *
    * Most of the format agnostic alphabet is missing here, which is the point. A collection, a dictionary and a union
    * have no CSV rendering, so this format simply does not name them.
    */
  sealed abstract class Schema[+S[-w, +r] <: Csv.Schema[?, w, r], -W, +R]

  /** A schema holding `S` and round tripping `A`. */
  type Of[S[-w, +r] <: Csv.Node[w, r], A] = Csv.Schema[S, A, A]

  /** The general form, whatever the schema holds. This is what an interpreter that accepts any node is written against.
    */
  type Node = [w, r] =>> Csv.Schema[?, w, r]

  /** The `S` of a node with nothing inside it. */
  type Leaf = Nothing

  /** The `S` of a node holding both an `S1` and an `S2`, which is what `:*` accumulates.
    *
    * Bounded to a cell rather than to a schema, because a cell is the only thing a CSV row ever accumulates.
    */
  type Or[S1[-w, +r] <: Csv.Cell.Node[w, r], S2[-w, +r] <: Csv.Cell.Node[w, r]] = [w, r] =>> S1[w, r] | S2[w, r]

  /** The [[Metadata.Namespace]] the CSV interpreters read their attributes from. */
  val Namespace: Metadata.Namespace = Metadata.Namespace("csv")

  /** The [[Absence]] a schema's metadata asks for. Asking for nothing is [[Absence.Empty]], because a row's columns are
    * fixed by its header: a field with nothing to write still owes its column, and writes it empty. This is where CSV
    * reads the shared vocabulary the other way round from JSON, which drops the key instead.
    */
  private[otter] def absence(metadata: Metadata): Absence =
    metadata.get(Csv.Namespace, Metadata.Namespace.Global, Keys.absence).getOrElse(Absence.Empty)

  /** The [[Tolerance]] a schema's metadata asks for. Asking for nothing is [[Tolerance.Lenient]], so that a field round
    * trips whether its column is missing or merely empty.
    */
  private[otter] def tolerance(metadata: Metadata): Tolerance =
    metadata.get(Csv.Namespace, Metadata.Namespace.Global, Keys.tolerance).getOrElse(Tolerance.Lenient)

  /** A schema that reads `A`, whatever it writes. */
  type Reader[+A] = Csv.Reader.Of[Csv.Node, A]

  object Reader:
    type Of[S[-w, +r] <: Csv.Node[w, r], +A] = Csv.Schema[S, Nothing, A]

  /** A schema that writes `A`, whatever it reads. */
  type Writer[-A] = Csv.Writer.Of[Csv.Node, A]

  object Writer:
    type Of[S[-w, +r] <: Csv.Node[w, r], -A] = Csv.Schema[S, A, Any]

  type Cell[A] = Csv.Cell.Of[Csv.Node, A]

  object Cell:
    /** A cell holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Csv.Node[w, r], A] = Csv.Cell.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Csv.Cell.Schema[Csv.Node, w, r]

    type Reader[+A] = Csv.Cell.Reader.Of[Csv.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Csv.Node[w, r], +A] = Csv.Cell.Schema[S, Nothing, A]

    type Writer[-A] = Csv.Cell.Writer.Of[Csv.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Csv.Node[w, r], -A] = Csv.Cell.Schema[S, A, Any]

    /** Everything that fits in one piece of text.
      *
      * The bound restates its parent's rather than narrowing to `Csv.Cell.Schema`. Narrowing reads as the tighter
      * statement but makes every wildcard a row's `S` is compared through recur one generation deeper, and it says
      * nothing the bound on `Csv.Record.Schema`'s own `S` does not already say.
      */
    sealed abstract class Schema[+S[-w, +r] <: Csv.Schema[?, w, r], -W, +R] extends Csv.Schema[S, W, R]

    given profunctor: [S[-w, +r] <: Csv.Node[w, r]] => Profunctor[[w, r] =>> Csv.Cell.Schema[S, w, r]]:
      override def dimap[W0, R0, W, R](
          self: Csv.Cell.Schema[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Csv.Cell.Schema[S, W, R] = self match
        case self @ Csv.Coerce.Schema(_)            => Csv.Coerce.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Constant.Schema(_)          => Csv.Constant.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Enumeration.Schema(_)       => Csv.Enumeration.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Optional.Schema(_)          => Csv.Optional.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Primitive.Boolean.Schema(_) => Csv.Primitive.Boolean.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Primitive.Number.Schema(_)  => Csv.Primitive.Number.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Primitive.Text.Schema(_)    => Csv.Primitive.Text.Schema.profunctor.dimap(self)(f)(g)

    given functor: [S[-w, +r] <: Csv.Node[w, r]] => Functor[[a] =>> Csv.Cell.Schema[S, Nothing, a]] =
      Direction.functor[[w, r] =>> Csv.Cell.Schema[S, w, r]]

    given contravariant: [S[-w, +r] <: Csv.Node[w, r]] => Contravariant[[a] =>> Csv.Cell.Schema[S, a, Any]] =
      Direction.contravariant[[w, r] =>> Csv.Cell.Schema[S, w, r]]

    given invariant: [S[-w, +r] <: Csv.Node[w, r]] => Invariant[[a] =>> Csv.Cell.Schema[S, a, a]] =
      Direction.invariant[[w, r] =>> Csv.Cell.Schema[S, w, r]]

  type Primitive[A] = Csv.Primitive.Of[Csv.Node, A]

  object Primitive:
    /** A primitive holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Csv.Node[w, r], A] = Csv.Primitive.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Csv.Primitive.Schema[Csv.Node, w, r]

    type Reader[+A] = Csv.Primitive.Reader.Of[Csv.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Csv.Node[w, r], +A] = Csv.Primitive.Schema[S, Nothing, A]

    type Writer[-A] = Csv.Primitive.Writer.Of[Csv.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Csv.Node[w, r], -A] = Csv.Primitive.Schema[S, A, Any]

    sealed abstract class Schema[+S[-w, +r] <: Csv.Schema[?, w, r], -W, +R] extends Csv.Cell.Schema[S, W, R]

    type Boolean[A] = Csv.Primitive.Boolean.Schema[A, A]

    object Boolean:
      /** Holding nothing, so `Node` is the schema itself. */
      type Node = [w, r] =>> Csv.Primitive.Boolean.Schema[w, r]

      type Reader[+A] = Csv.Primitive.Boolean.Schema[Nothing, A]

      type Writer[-A] = Csv.Primitive.Boolean.Schema[A, Any]

      final case class Schema[-W, +R](self: Annotation[Self.Primitive.Boolean[W, R]])
          extends Csv.Primitive.Schema[Csv.Leaf, W, R]

      object Schema
          extends Wrapper.Primitive.Boolean[Csv.Primitive.Boolean.Schema](
            [w, r] =>
              (annotation: Annotation[Self.Primitive.Boolean[w, r]]) => new Csv.Primitive.Boolean.Schema(annotation),
            [w, r] => (csv: Csv.Primitive.Boolean.Schema[w, r]) => csv.self
          )

    type Number[A] = Csv.Primitive.Number.Schema[A, A]

    object Number:
      /** Holding nothing, so `Node` is the schema itself. */
      type Node = [w, r] =>> Csv.Primitive.Number.Schema[w, r]

      type Reader[+A] = Csv.Primitive.Number.Schema[Nothing, A]

      type Writer[-A] = Csv.Primitive.Number.Schema[A, Any]

      final case class Schema[-W, +R](self: Annotation[Self.Primitive.Number[W, R]])
          extends Csv.Primitive.Schema[Csv.Leaf, W, R]

      object Schema
          extends Wrapper.Primitive.Number[Csv.Primitive.Number.Schema](
            [w, r] =>
              (annotation: Annotation[Self.Primitive.Number[w, r]]) => new Csv.Primitive.Number.Schema(annotation),
            [w, r] => (csv: Csv.Primitive.Number.Schema[w, r]) => csv.self
          )

    type Text[A] = Csv.Primitive.Text.Schema[A, A]

    object Text:
      /** Holding nothing, so `Node` is the schema itself. */
      type Node = [w, r] =>> Csv.Primitive.Text.Schema[w, r]

      type Reader[+A] = Csv.Primitive.Text.Schema[Nothing, A]

      type Writer[-A] = Csv.Primitive.Text.Schema[A, Any]

      final case class Schema[-W, +R](self: Annotation[Self.Primitive.Text[W, R]])
          extends Csv.Primitive.Schema[Csv.Leaf, W, R]

      object Schema
          extends Wrapper.Primitive.Text[Csv.Primitive.Text.Schema](
            [w, r] => (annotation: Annotation[Self.Primitive.Text[w, r]]) => new Csv.Primitive.Text.Schema(annotation),
            [w, r] => (csv: Csv.Primitive.Text.Schema[w, r]) => csv.self
          )

    given profunctor: [S[-w, +r] <: Csv.Node[w, r]] => Profunctor[[w, r] =>> Csv.Primitive.Schema[S, w, r]]:
      override def dimap[W0, R0, W, R](
          self: Csv.Primitive.Schema[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Csv.Primitive.Schema[S, W, R] = self match
        case self @ Csv.Primitive.Boolean.Schema(_) => Csv.Primitive.Boolean.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Primitive.Number.Schema(_)  => Csv.Primitive.Number.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Primitive.Text.Schema(_)    => Csv.Primitive.Text.Schema.profunctor.dimap(self)(f)(g)

    given functor: [S[-w, +r] <: Csv.Node[w, r]] => Functor[[a] =>> Csv.Primitive.Schema[S, Nothing, a]] =
      Direction.functor[[w, r] =>> Csv.Primitive.Schema[S, w, r]]

    given contravariant: [S[-w, +r] <: Csv.Node[w, r]] => Contravariant[[a] =>> Csv.Primitive.Schema[S, a, Any]] =
      Direction.contravariant[[w, r] =>> Csv.Primitive.Schema[S, w, r]]

    given invariant: [S[-w, +r] <: Csv.Node[w, r]] => Invariant[[a] =>> Csv.Primitive.Schema[S, a, a]] =
      Direction.invariant[[w, r] =>> Csv.Primitive.Schema[S, w, r]]

  type Coerce[A] = Csv.Coerce.Of[Csv.Primitive.Node, A]

  object Coerce:
    /** A coercion holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Csv.Primitive.Node[w, r], A] = Csv.Coerce.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Csv.Coerce.Schema[Csv.Primitive.Node, w, r]

    type Reader[+A] = Csv.Coerce.Reader.Of[Csv.Primitive.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Csv.Primitive.Node[w, r], +A] = Csv.Coerce.Schema[S, Nothing, A]

    type Writer[-A] = Csv.Coerce.Writer.Of[Csv.Primitive.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Csv.Primitive.Node[w, r], -A] = Csv.Coerce.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Csv.Primitive.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Coerce[S, W, R]]
    ) extends Csv.Cell.Schema[S, W, R]

    object Schema
        extends Wrapper.Coerce[Csv.Primitive.Node, Csv.Coerce.Schema](
          [s[-w, +r] <: Csv.Primitive.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Coerce[s, w, r]]) => new Csv.Coerce.Schema(annotation),
          [s[-w, +r] <: Csv.Primitive.Node[w, r], w, r] => (csv: Csv.Coerce.Schema[s, w, r]) => csv.self
        )

  type Constant[A] = Csv.Constant.Of[Csv.Primitive.Node, A]

  object Constant:
    /** A constant holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Csv.Primitive.Node[w, r], A] = Csv.Constant.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Csv.Constant.Schema[Csv.Primitive.Node, w, r]

    type Reader[+A] = Csv.Constant.Reader.Of[Csv.Primitive.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Csv.Primitive.Node[w, r], +A] = Csv.Constant.Schema[S, Nothing, A]

    type Writer[-A] = Csv.Constant.Writer.Of[Csv.Primitive.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Csv.Primitive.Node[w, r], -A] = Csv.Constant.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Csv.Primitive.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Constant[S, W, R]]
    ) extends Csv.Cell.Schema[S, W, R]

    object Schema
        extends Wrapper.Constant[Csv.Primitive.Node, Csv.Constant.Schema](
          [s[-w, +r] <: Csv.Primitive.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Constant[s, w, r]]) => new Csv.Constant.Schema(annotation),
          [s[-w, +r] <: Csv.Primitive.Node[w, r], w, r] => (csv: Csv.Constant.Schema[s, w, r]) => csv.self
        )

  type Enumeration[A] = Csv.Enumeration.Of[Csv.Primitive.Node, A]

  object Enumeration:
    /** An enumeration holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Csv.Primitive.Node[w, r], A] = Csv.Enumeration.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Csv.Enumeration.Schema[Csv.Primitive.Node, w, r]

    type Reader[+A] = Csv.Enumeration.Reader.Of[Csv.Primitive.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Csv.Primitive.Node[w, r], +A] = Csv.Enumeration.Schema[S, Nothing, A]

    type Writer[-A] = Csv.Enumeration.Writer.Of[Csv.Primitive.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Csv.Primitive.Node[w, r], -A] = Csv.Enumeration.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Csv.Primitive.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Enumeration[S, W, R]]
    ) extends Csv.Cell.Schema[S, W, R]

    object Schema
        extends Wrapper.Enumeration[Csv.Primitive.Node, Csv.Enumeration.Schema](
          [s[-w, +r] <: Csv.Primitive.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Enumeration[s, w, r]]) => new Csv.Enumeration.Schema(annotation),
          [s[-w, +r] <: Csv.Primitive.Node[w, r], w, r] => (csv: Csv.Enumeration.Schema[s, w, r]) => csv.self
        )

  type Optional[A] = Csv.Optional.Of[Csv.Cell.Node, A]

  object Optional:
    /** An optional cell holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Csv.Cell.Node[w, r], A] = Csv.Optional.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Csv.Optional.Schema[Csv.Cell.Node, w, r]

    type Reader[+A] = Csv.Optional.Reader.Of[Csv.Cell.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Csv.Cell.Node[w, r], +A] = Csv.Optional.Schema[S, Nothing, A]

    type Writer[-A] = Csv.Optional.Writer.Of[Csv.Cell.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Csv.Cell.Node[w, r], -A] = Csv.Optional.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Csv.Cell.Schema[?, w, r], -W, +R](self: Annotation[Self.Optional[S, W, R]])
        extends Csv.Cell.Schema[S, W, R]

    object Schema
        extends Wrapper.Optional[Csv.Cell.Node, Csv.Optional.Schema](
          [s[-w, +r] <: Csv.Cell.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Optional[s, w, r]]) => new Csv.Optional.Schema(annotation),
          [s[-w, +r] <: Csv.Cell.Node[w, r], w, r] => (csv: Csv.Optional.Schema[s, w, r]) => csv.self
        )

  type Record[A] = Csv.Record.Of[Csv.Cell.Node, A]

  object Record:
    /** A keyed row holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Csv.Cell.Node[w, r], A] = Csv.Record.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Csv.Record.Schema[Csv.Cell.Node, w, r]

    type Reader[+A] = Csv.Record.Reader.Of[Csv.Cell.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Csv.Cell.Node[w, r], +A] = Csv.Record.Schema[S, Nothing, A]

    type Writer[-A] = Csv.Record.Writer.Of[Csv.Cell.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Csv.Cell.Node[w, r], -A] = Csv.Record.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Csv.Cell.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Record[[w, r] =>> Csv.Field.Schema[S, w, r], W, R]]
    ) extends Csv.Schema[S, W, R]

    object Schema
        extends Wrapper.Record[Csv.Cell.Node, Csv.Record.Schema, Csv.Field.Schema](
          [s[-w, +r] <: Csv.Cell.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Record[[a, b] =>> Csv.Field.Schema[s, a, b], w, r]]) =>
              new Csv.Record.Schema(annotation),
          [s[-w, +r] <: Csv.Cell.Node[w, r], w, r] => (csv: Csv.Record.Schema[s, w, r]) => csv.self
        ):
      given recordable: [S[-w, +r] <: Csv.Cell.Node[w, r]]
        => RecordableOperation[[w, r] =>> Csv.Record.Schema[S, w, r], [w, r] =>> Csv.Record.Schema[S, w, r]] =
        RecordableOperation.identity

      /** `record :* field`. The result carries both children's `S`, so the union accumulates down the chain. */
      given appendable: [S1[-w, +r] <: Csv.Cell.Node[w, r], S2[-w, +r] <: Csv.Cell.Node[w, r]]
          => AppendableOperation[
            [w, r] =>> Csv.Record.Schema[S1, w, r],
            [w, r] =>> Csv.Record.Schema[Csv.Or[S1, S2], w, r],
            [w, r] =>> Csv.Field.Schema[S2, w, r]
          ]:
        override def lift[W, R](fa: Csv.Record.Schema[S1, W, R]): Csv.Record.Schema[Csv.Or[S1, S2], W, R] = fa

        override def element[W, R](fb: => Csv.Field.Schema[S2, W, R]): Csv.Record.Schema[Csv.Or[S1, S2], W, R] =
          Csv.Record.Schema.apply[Csv.Or[S1, S2], W, R](Self.Record.Root(Reference.later(fb)))

  type Tuple[A] = Csv.Tuple.Of[Csv.Cell.Node, A]

  object Tuple:
    /** A positional row holding `S` and round tripping `A`. A CSV file without a header addresses its cells by index,
      * which is what makes this the same row as [[Csv.Record]] with the names taken away.
      */
    type Of[S[-w, +r] <: Csv.Cell.Node[w, r], A] = Csv.Tuple.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Csv.Tuple.Schema[Csv.Cell.Node, w, r]

    type Reader[+A] = Csv.Tuple.Reader.Of[Csv.Cell.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Csv.Cell.Node[w, r], +A] = Csv.Tuple.Schema[S, Nothing, A]

    type Writer[-A] = Csv.Tuple.Writer.Of[Csv.Cell.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Csv.Cell.Node[w, r], -A] = Csv.Tuple.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Csv.Cell.Schema[?, w, r], -W, +R](self: Annotation[Self.Tuple[S, W, R]])
        extends Csv.Schema[S, W, R]

    object Schema
        extends Wrapper.Tuple[Csv.Cell.Node, Csv.Tuple.Schema](
          [s[-w, +r] <: Csv.Cell.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Tuple[s, w, r]]) => new Csv.Tuple.Schema(annotation),
          [s[-w, +r] <: Csv.Cell.Node[w, r], w, r] => (csv: Csv.Tuple.Schema[s, w, r]) => csv.self
        ):
      given tupleable: [S[-w, +r] <: Csv.Cell.Node[w, r]]
        => TupleableOperation[[w, r] =>> Csv.Tuple.Schema[S, w, r], [w, r] =>> Csv.Tuple.Schema[S, w, r]] =
        TupleableOperation.identity

      /** `tuple :* cell`. A tuple's members are cells themselves, not fields, so nothing names them. */
      given appendable: [S1[-w, +r] <: Csv.Cell.Node[w, r], S2[-w, +r] <: Csv.Cell.Node[w, r]]
          => AppendableOperation[
            [w, r] =>> Csv.Tuple.Schema[S1, w, r],
            [w, r] =>> Csv.Tuple.Schema[Csv.Or[S1, S2], w, r],
            S2
          ]:
        override def lift[W, R](fa: Csv.Tuple.Schema[S1, W, R]): Csv.Tuple.Schema[Csv.Or[S1, S2], W, R] = fa

        override def element[W, R](fb: => S2[W, R]): Csv.Tuple.Schema[Csv.Or[S1, S2], W, R] =
          Csv.Tuple.Schema.apply[Csv.Or[S1, S2], W, R](Self.Tuple.Root(Reference.later(fb)))

  type Field[A] = Csv.Field.Of[Csv.Cell.Node, A]

  object Field:
    /** A column holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Csv.Cell.Node[w, r], A] = Csv.Field.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Csv.Field.Schema[Csv.Cell.Node, w, r]

    type Reader[+A] = Csv.Field.Reader.Of[Csv.Cell.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Csv.Cell.Node[w, r], +A] = Csv.Field.Schema[S, Nothing, A]

    type Writer[-A] = Csv.Field.Writer.Of[Csv.Cell.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Csv.Cell.Node[w, r], -A] = Csv.Field.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Csv.Cell.Schema[?, w, r], -W, +R](self: Annotation[Self.Field[S, W, R]])

    object Schema
        extends Wrapper.Field[Csv.Cell.Node, Csv.Field.Schema](
          [s[-w, +r] <: Csv.Cell.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Field[s, w, r]]) => new Csv.Field.Schema(annotation),
          [s[-w, +r] <: Csv.Cell.Node[w, r], w, r] => (csv: Csv.Field.Schema[s, w, r]) => csv.self
        ):
      given recordable: [S[-w, +r] <: Csv.Cell.Node[w, r]]
        => RecordableOperation[[w, r] =>> Csv.Field.Schema[S, w, r], [w, r] =>> Csv.Record.Schema[S, w, r]] =
        RecordableOperation.derived

      /** `field :* field`. */
      given appendable: [S1[-w, +r] <: Csv.Cell.Node[w, r], S2[-w, +r] <: Csv.Cell.Node[w, r]]
          => AppendableOperation[
            [w, r] =>> Csv.Field.Schema[S1, w, r],
            [w, r] =>> Csv.Record.Schema[Csv.Or[S1, S2], w, r],
            [w, r] =>> Csv.Field.Schema[S2, w, r]
          ]:
        override def lift[W, R](fa: Csv.Field.Schema[S1, W, R]): Csv.Record.Schema[Csv.Or[S1, S2], W, R] =
          Csv.Record.Schema.apply[Csv.Or[S1, S2], W, R](Self.Record.Root(Reference.now(fa)))

        override def element[W, R](fb: => Csv.Field.Schema[S2, W, R]): Csv.Record.Schema[Csv.Or[S1, S2], W, R] =
          Csv.Record.Schema.apply[Csv.Or[S1, S2], W, R](Self.Record.Root(Reference.later(fb)))

  given profunctor: [S[-w, +r] <: Csv.Node[w, r]] => Profunctor[[w, r] =>> Csv.Schema[S, w, r]]:
    override def dimap[W0, R0, W, R](self: Csv.Schema[S, W0, R0])(f: W => W0)(g: R0 => R): Csv.Schema[S, W, R] =
      self match
        case self @ Csv.Coerce.Schema(_)            => Csv.Coerce.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Constant.Schema(_)          => Csv.Constant.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Enumeration.Schema(_)       => Csv.Enumeration.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Optional.Schema(_)          => Csv.Optional.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Primitive.Boolean.Schema(_) => Csv.Primitive.Boolean.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Primitive.Number.Schema(_)  => Csv.Primitive.Number.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Primitive.Text.Schema(_)    => Csv.Primitive.Text.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Record.Schema(_)            => Csv.Record.Schema.profunctor.dimap(self)(f)(g)
        case self @ Csv.Tuple.Schema(_)             => Csv.Tuple.Schema.profunctor.dimap(self)(f)(g)

  given functor: [S[-w, +r] <: Csv.Node[w, r]] => Functor[[a] =>> Csv.Schema[S, Nothing, a]] =
    Direction.functor[[w, r] =>> Csv.Schema[S, w, r]]

  given contravariant: [S[-w, +r] <: Csv.Node[w, r]] => Contravariant[[a] =>> Csv.Schema[S, a, Any]] =
    Direction.contravariant[[w, r] =>> Csv.Schema[S, w, r]]

  given invariant: [S[-w, +r] <: Csv.Node[w, r]] => Invariant[[a] =>> Csv.Schema[S, a, a]] =
    Direction.invariant[[w, r] =>> Csv.Schema[S, w, r]]

  /** `S` is bounded to a cell rather than to a schema, so that these are not also offered on a row. A row is not
    * something a cell can hold, so neither an optional row nor a row lifted into a positional one means anything; a
    * field is not a schema at all and carries its own `optional` from [[Wrapper.Field]].
    */
  given optionalable: [S[-w, +r] <: Csv.Cell.Node[w, r]]
    => OptionalableOperation[S, [w, r] =>> Csv.Optional.Schema[S, w, r]] = OptionalableOperation.derived

  given tupleable: [S[-w, +r] <: Csv.Cell.Node[w, r]]
    => TupleableOperation[S, [w, r] =>> Csv.Tuple.Schema[S, w, r]] = TupleableOperation.derived

  /** `cell :* cell`, and `cell *: cell`: two cells beside each other are the positional row that holds them, which is
    * what [[io.taig.otter.component.TupleComponent.TNil]] would otherwise have to be named for.
    *
    * No guard, unlike [[Json.appendable]]: a positional row is not a cell, so a receiver that already is one falls
    * outside this instance's bound and keeps appending into itself through [[Csv.Tuple.Schema.appendable]].
    */
  given appendable: [S1[-w, +r] <: Csv.Cell.Node[w, r], S2[-w, +r] <: Csv.Cell.Node[w, r]]
      => AppendableOperation[S1, [w, r] =>> Csv.Tuple.Schema[Csv.Or[S1, S2], w, r], S2]:
    override def lift[W, R](fa: S1[W, R]): Csv.Tuple.Schema[Csv.Or[S1, S2], W, R] =
      Csv.Tuple.Schema.apply[Csv.Or[S1, S2], W, R](Self.Tuple.Root(Reference.now(fa)))

    override def element[W, R](fb: => S2[W, R]): Csv.Tuple.Schema[Csv.Or[S1, S2], W, R] =
      Csv.Tuple.Schema.apply[Csv.Or[S1, S2], W, R](Self.Tuple.Root(Reference.later(fb)))
