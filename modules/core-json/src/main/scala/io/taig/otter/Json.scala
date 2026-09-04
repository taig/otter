package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.arrow.Profunctor
import cats.data.NonEmptyList
import io.taig.otter as Self
import io.taig.otter.operation.*

/** A JSON schema that round trips `A`.
  *
  * Every node reads the same way. `Json.Record[A]` round trips, `Json.Record.Reader[A]` reads and
  * `Json.Record.Writer[A]` writes, each leaving open what the record holds. `Json.Record.Of[S, A]` pins that down, and
  * `Json.Record.Reader.Of[S, A]` does both at once. `Json.Record.Schema[+S, -W, +R]` is the class they all abbreviate.
  */
type Json[A] = Json.Of[Json.Node, A]

object Json:
  /** The JSON schema alphabet.
    *
    * Each member wraps one format agnostic node in an [[Annotation]] and ties the recursive knot by referring back to
    * `Json.Schema`. `W` is the type this schema writes, `R` the type it reads, and `S` the type of what is inside it:
    * the element of a collection, the fields of a record, the branches of a union.
    *
    * `S` is what makes a restricted schema expressible. `Json.Record.Of[Json.Primitive.Node, A]` is a record whose
    * fields are all primitives, so a conversion into a flat format can demand exactly that and reject anything nested.
    * It is not a witness the DSL maintains by hand: a field of type `Json.Field.Schema[S, W, R]` genuinely holds an
    * `S`.
    */
  sealed abstract class Schema[+S[-w, +r] <: Json.Schema[?, w, r], -W, +R]

  /** A schema holding `S` and round tripping `A`. `Json[A]` is this with `S` left open. */
  type Of[S[-w, +r] <: Json.Node[w, r], A] = Json.Schema[S, A, A]

  /** The general form, whatever the schema holds. This is what an interpreter that accepts any node is written against:
    * `Decoder[Json.Node, CirceJson]`.
    */
  type Node = [w, r] =>> Json.Schema[?, w, r]

  /** The `S` of a node with nothing inside it. It is the bottom of the constructor lattice, so a leaf widens to fit
    * wherever a child is expected, exactly as `Record.Empty` does in the format agnostic layer.
    */
  type Leaf = Nothing

  /** The `S` of a node holding both an `S1` and an `S2`, which is what `:*` and `:+` accumulate.
    *
    * Leave both sides to inference. Ascribing an operand to [[Json.Node]] asks the compiler to decide
    * `S1[w, r] | S2[w, r] <: Json.Schema[?, w, r]` before it has solved either side, which dotty answers with an
    * `AssertionError` out of `TypeOps.orDominator` rather than with a type error. Naming a constructor it can already
    * see is fine -- `Json.Record.Node`, for a branch that holds any record -- naming the top of the lattice is not.
    */
  type Or[S1[-w, +r] <: Json.Node[w, r], S2[-w, +r] <: Json.Node[w, r]] = [w, r] =>> S1[w, r] | S2[w, r]

  /** The [[Metadata.Namespace]] the JSON interpreters read their attributes from.
    *
    * An attribute set here wins over the same attribute set globally, so a schema can say one thing to every format and
    * another to JSON alone.
    */
  val Namespace: Metadata.Namespace = Metadata.Namespace("json")

  /** The [[Absence]] a schema's metadata asks for. Asking for nothing is [[Absence.Omit]], so a field that says nothing
    * about absence drops its key, the way a field always has.
    */
  private[otter] def absence(metadata: Metadata): Absence =
    metadata.get(Json.Namespace, Metadata.Namespace.Global, Keys.absence).getOrElse(Absence.Omit)

  /** The [[Tolerance]] a schema's metadata asks for. Asking for nothing is [[Tolerance.Lenient]], so that a field round
    * trips whichever way it is written.
    */
  private[otter] def tolerance(metadata: Metadata): Tolerance =
    metadata.get(Json.Namespace, Metadata.Namespace.Global, Keys.tolerance).getOrElse(Tolerance.Lenient)

  /** The [[Metadata]] of a node, whichever node it is.
    *
    * Every wrapper in the alphabet holds an [[Annotation]], but they hold it at their own type and `Json.Schema` itself
    * declares no accessor, so reaching it is the same exhaustive match `Json.profunctor` is.
    */
  def metadata(json: Json.Node[?, ?]): Metadata = json match
    case Json.Coerce.Schema(node)            => node.metadata
    case Json.Collection.Schema(node)        => node.metadata
    case Json.Constant.Schema(node)          => node.metadata
    case Json.Dictionary.Schema(node)        => node.metadata
    case Json.Enumeration.Schema(node)       => node.metadata
    case Json.Optional.Schema(node)          => node.metadata
    case Json.Primitive.Boolean.Schema(node) => node.metadata
    case Json.Primitive.Number.Schema(node)  => node.metadata
    case Json.Primitive.Text.Schema(node)    => node.metadata
    case Json.Record.Schema(node)            => node.metadata
    case Json.Tuple.Schema(node)             => node.metadata
    case Json.Union.Schema(node)             => node.metadata

  /** An attribute under the first of `namespaces` that has one, so that a target specific override wins over a format
    * specific one, which wins over a global one.
    */
  def attr[A](namespaces: NonEmptyList[Metadata.Namespace], metadata: Metadata, key: Metadata.Key[A]): Option[A] =
    metadata.get(namespaces.head, namespaces.tail*)(key)

  /** The name a node asks to be declared under. A node without one is rendered where it stands. */
  def name(namespaces: NonEmptyList[Metadata.Namespace], json: Json.Node[?, ?]): Option[String] =
    Json.attr(namespaces, Json.metadata(json), Keys.name)

  /** How a record's key behaves, which is the one place the two sides of a schema disagree about a shape rather than
    * about a value.
    *
    * The four cases are what a JSON object can say about a member: it is there, it is there holding nothing, it may not
    * be there, or it may not be there and may hold nothing when it is.
    */
  enum Presence:
    case Required, Nullable, Optional, OptionalNullable

  /** What [[Presence]] a field has, on one side.
    *
    * Read off the circe interpreters, which are the definition of the wire. A field that drops its key when absent is
    * written as [[Presence.Optional]] and read, leniently, as [[Presence.OptionalNullable]], because a lenient field
    * takes a missing key and an explicit empty alike. A field holding a default is never absent when written and always
    * may be when read, which is the same asymmetry the other way round.
    */
  def presence[F[-_, +_], W, R](side: Side, metadata: Metadata, field: Self.Field[F, W, R]): Json.Presence =
    field match
      case Self.Field.Root(_, _)         => Json.Presence.Required
      case Self.Field.Modify(self, _, _) => Json.presence(side, metadata, self)
      case Self.Field.Default(self, _)   =>
        side match
          case Side.Write => Json.Presence.Required
          case Side.Read  => Json.absent(metadata)
      case Self.Field.Optional(self) =>
        side match
          case Side.Write =>
            Json.absence(metadata) match
              case Absence.Omit  => Json.Presence.Optional
              case Absence.Empty => Json.Presence.Nullable
          case Side.Read => Json.absent(metadata)

  /** Whether a tuple admits the all empty form the codecs use for a tuple that is not there.
    *
    * The same asymmetry a field has, positionally: a tuple holding a default is always written in full and may arrive
    * empty, and one that is optional may be either way round.
    *
    * Every position has to admit it, which is why a product is a conjunction. The decoder tests the whole array against
    * `empty` and then reads each position out of it, so a single position that insists on a value is enough to make the
    * all empty form unreadable -- and a renderer that says otherwise describes a document nothing can satisfy.
    */
  def absent[F[-_, +_], W, R](side: Side, schema: Self.Tuple[F, W, R]): Boolean = schema match
    case Self.Tuple.Empty                => true
    case Self.Tuple.Root(_)              => false
    case Self.Tuple.Product(left, right) => Json.absent(side, left) && Json.absent(side, right)
    case Self.Tuple.Optional(_)          => true
    case Self.Tuple.Modify(self, _, _)   => Json.absent(side, self)
    case Self.Tuple.Default(_, _)        =>
      side match
        case Side.Write => false
        case Side.Read  => true

  /** Which forms of absence a field accepts when read. */
  private def absent(metadata: Metadata): Json.Presence = Json.tolerance(metadata) match
    case Tolerance.Lenient => Json.Presence.OptionalNullable
    case Tolerance.Strict  =>
      Json.absence(metadata) match
        case Absence.Omit  => Json.Presence.Optional
        case Absence.Empty => Json.Presence.Nullable

  /** A schema that reads `A`, whatever it writes.
    *
    * Every node carries the same pair. `Json.Record[A] <: Json.Record.Reader[A]`, so a round tripping schema is
    * accepted wherever a reader is asked for.
    */
  type Reader[+A] = Json.Reader.Of[Json.Node, A]

  object Reader:
    /** A reader that additionally says what it holds. */
    type Of[S[-w, +r] <: Json.Node[w, r], +A] = Json.Schema[S, Nothing, A]

  /** A schema that writes `A`, whatever it reads. */
  type Writer[-A] = Json.Writer.Of[Json.Node, A]

  object Writer:
    /** A writer that additionally says what it holds. */
    type Of[S[-w, +r] <: Json.Node[w, r], -A] = Json.Schema[S, A, Any]

  type Coerce[A] = Json.Coerce.Of[Json.Primitive.Node, A]

  object Coerce:
    /** A coercion holding `S` and round tripping `A`. `Json.Coerce[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Primitive.Node[w, r], A] = Json.Coerce.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Coerce.Schema[Json.Primitive.Node, w, r]

    type Reader[+A] = Json.Coerce.Reader.Of[Json.Primitive.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Primitive.Node[w, r], +A] = Json.Coerce.Schema[S, Nothing, A]

    type Writer[-A] = Json.Coerce.Writer.Of[Json.Primitive.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Primitive.Node[w, r], -A] = Json.Coerce.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Primitive.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Coerce[S, W, R]]
    ) extends Json.Schema[S, W, R]

    object Schema
        extends Wrapper.Coerce[Json.Primitive.Node, Json.Coerce.Schema](
          [s[-w, +r] <: Json.Primitive.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Coerce[s, w, r]]) => new Json.Coerce.Schema(annotation),
          [s[-w, +r] <: Json.Primitive.Node[w, r], w, r] => (json: Json.Coerce.Schema[s, w, r]) => json.self
        )

  type Collection[A] = Json.Collection.Of[Json.Node, A]

  object Collection:
    /** A collection holding `S` and round tripping `A`. `Json.Collection[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Node[w, r], A] = Json.Collection.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Collection.Schema[Json.Node, w, r]

    type Reader[+A] = Json.Collection.Reader.Of[Json.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Node[w, r], +A] = Json.Collection.Schema[S, Nothing, A]

    type Writer[-A] = Json.Collection.Writer.Of[Json.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Node[w, r], -A] = Json.Collection.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Schema[?, w, r], -W, +R](self: Annotation[Self.Collection[S, W, R]])
        extends Json.Schema[S, W, R]

    object Schema
        extends Wrapper.Collection[Json.Node, Json.Collection.Schema](
          [s[-w, +r] <: Json.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Collection[s, w, r]]) => new Json.Collection.Schema(annotation),
          [s[-w, +r] <: Json.Node[w, r], w, r] => (json: Json.Collection.Schema[s, w, r]) => json.self
        )

  type Constant[A] = Json.Constant.Of[Json.Primitive.Node, A]

  object Constant:
    /** A constant holding `S` and round tripping `A`. `Json.Constant[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Primitive.Node[w, r], A] = Json.Constant.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Constant.Schema[Json.Primitive.Node, w, r]

    type Reader[+A] = Json.Constant.Reader.Of[Json.Primitive.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Primitive.Node[w, r], +A] = Json.Constant.Schema[S, Nothing, A]

    type Writer[-A] = Json.Constant.Writer.Of[Json.Primitive.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Primitive.Node[w, r], -A] = Json.Constant.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Primitive.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Constant[S, W, R]]
    ) extends Json.Schema[S, W, R]

    object Schema
        extends Wrapper.Constant[Json.Primitive.Node, Json.Constant.Schema](
          [s[-w, +r] <: Json.Primitive.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Constant[s, w, r]]) => new Json.Constant.Schema(annotation),
          [s[-w, +r] <: Json.Primitive.Node[w, r], w, r] => (json: Json.Constant.Schema[s, w, r]) => json.self
        )

  type Dictionary[A] = Json.Dictionary.Of[Json.Node, A]

  object Dictionary:
    /** A dictionary holding `S` and round tripping `A`. `Json.Dictionary[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Node[w, r], A] = Json.Dictionary.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Dictionary.Schema[Json.Node, w, r]

    type Reader[+A] = Json.Dictionary.Reader.Of[Json.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Node[w, r], +A] = Json.Dictionary.Schema[S, Nothing, A]

    type Writer[-A] = Json.Dictionary.Writer.Of[Json.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Node[w, r], -A] = Json.Dictionary.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Dictionary[Json.Primitive.Text.Node, S, W, R]]
    ) extends Json.Schema[S, W, R]

    object Schema
        extends Wrapper.Dictionary[Json.Node, Json.Primitive.Text.Node, Json.Dictionary.Schema](
          [s[-w, +r] <: Json.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Dictionary[Json.Primitive.Text.Node, s, w, r]]) =>
              new Json.Dictionary.Schema(annotation),
          [s[-w, +r] <: Json.Node[w, r], w, r] => (json: Json.Dictionary.Schema[s, w, r]) => json.self
        )

  type Enumeration[A] = Json.Enumeration.Of[Json.Primitive.Node, A]

  object Enumeration:
    /** An enumeration holding `S` and round tripping `A`. `Json.Enumeration[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Primitive.Node[w, r], A] = Json.Enumeration.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Enumeration.Schema[Json.Primitive.Node, w, r]

    type Reader[+A] = Json.Enumeration.Reader.Of[Json.Primitive.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Primitive.Node[w, r], +A] = Json.Enumeration.Schema[S, Nothing, A]

    type Writer[-A] = Json.Enumeration.Writer.Of[Json.Primitive.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Primitive.Node[w, r], -A] = Json.Enumeration.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Primitive.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Enumeration[S, W, R]]
    ) extends Json.Schema[S, W, R]

    object Schema
        extends Wrapper.Enumeration[Json.Primitive.Node, Json.Enumeration.Schema](
          [s[-w, +r] <: Json.Primitive.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Enumeration[s, w, r]]) => new Json.Enumeration.Schema(annotation),
          [s[-w, +r] <: Json.Primitive.Node[w, r], w, r] => (json: Json.Enumeration.Schema[s, w, r]) => json.self
        )

  type Optional[A] = Json.Optional.Of[Json.Node, A]

  object Optional:
    /** An optional schema holding `S` and round tripping `A`. `Json.Optional[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Node[w, r], A] = Json.Optional.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Optional.Schema[Json.Node, w, r]

    type Reader[+A] = Json.Optional.Reader.Of[Json.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Node[w, r], +A] = Json.Optional.Schema[S, Nothing, A]

    type Writer[-A] = Json.Optional.Writer.Of[Json.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Node[w, r], -A] = Json.Optional.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Schema[?, w, r], -W, +R](self: Annotation[Self.Optional[S, W, R]])
        extends Json.Schema[S, W, R]

    object Schema
        extends Wrapper.Optional[Json.Node, Json.Optional.Schema](
          [s[-w, +r] <: Json.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Optional[s, w, r]]) => new Json.Optional.Schema(annotation),
          [s[-w, +r] <: Json.Node[w, r], w, r] => (json: Json.Optional.Schema[s, w, r]) => json.self
        )

  type Record[A] = Json.Record.Of[Json.Node, A]

  object Record:
    /** A record holding `S` and round tripping `A`. `Json.Record[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Node[w, r], A] = Json.Record.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Record.Schema[Json.Node, w, r]

    type Reader[+A] = Json.Record.Reader.Of[Json.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Node[w, r], +A] = Json.Record.Schema[S, Nothing, A]

    type Writer[-A] = Json.Record.Writer.Of[Json.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Node[w, r], -A] = Json.Record.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Record[[w, r] =>> Json.Field.Schema[S, w, r], W, R]]
    ) extends Json.Schema[S, W, R]

    object Schema
        extends Wrapper.Record[Json.Node, Json.Record.Schema, Json.Field.Schema](
          [s[-w, +r] <: Json.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Record[[a, b] =>> Json.Field.Schema[s, a, b], w, r]]) =>
              new Json.Record.Schema(annotation),
          [s[-w, +r] <: Json.Node[w, r], w, r] => (json: Json.Record.Schema[s, w, r]) => json.self
        ):
      given recordable: [S[-w, +r] <: Json.Node[w, r]]
        => RecordableOperation[[w, r] =>> Json.Record.Schema[S, w, r], [w, r] =>> Json.Record.Schema[S, w, r]] =
        RecordableOperation.identity

      /** `record :* field`. The result carries both children's `S`, so the union accumulates down the chain. */
      given appendable: [S1[-w, +r] <: Json.Node[w, r], S2[-w, +r] <: Json.Node[w, r]]
          => AppendableOperation[
            [w, r] =>> Json.Record.Schema[S1, w, r],
            [w, r] =>> Json.Record.Schema[Json.Or[S1, S2], w, r],
            [w, r] =>> Json.Field.Schema[S2, w, r]
          ]:
        override def lift[W, R](fa: Json.Record.Schema[S1, W, R]): Json.Record.Schema[Json.Or[S1, S2], W, R] = fa

        override def element[W, R](fb: => Json.Field.Schema[S2, W, R]): Json.Record.Schema[Json.Or[S1, S2], W, R] =
          Json.Record.Schema.apply[Json.Or[S1, S2], W, R](Self.Record.Root(Reference.later(fb)))

  type Tuple[A] = Json.Tuple.Of[Json.Node, A]

  object Tuple:
    /** A tuple holding `S` and round tripping `A`. `Json.Tuple[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Node[w, r], A] = Json.Tuple.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Tuple.Schema[Json.Node, w, r]

    type Reader[+A] = Json.Tuple.Reader.Of[Json.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Node[w, r], +A] = Json.Tuple.Schema[S, Nothing, A]

    type Writer[-A] = Json.Tuple.Writer.Of[Json.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Node[w, r], -A] = Json.Tuple.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Schema[?, w, r], -W, +R](self: Annotation[Self.Tuple[S, W, R]])
        extends Json.Schema[S, W, R]

    object Schema
        extends Wrapper.Tuple[Json.Node, Json.Tuple.Schema](
          [s[-w, +r] <: Json.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Tuple[s, w, r]]) => new Json.Tuple.Schema(annotation),
          [s[-w, +r] <: Json.Node[w, r], w, r] => (json: Json.Tuple.Schema[s, w, r]) => json.self
        ):
      given tupleable: [S[-w, +r] <: Json.Node[w, r]]
        => TupleableOperation[[w, r] =>> Json.Tuple.Schema[S, w, r], [w, r] =>> Json.Tuple.Schema[S, w, r]] =
        TupleableOperation.identity

      /** `tuple :* schema`. */
      given appendable: [S1[-w, +r] <: Json.Node[w, r], S2[-w, +r] <: Json.Node[w, r]]
          => AppendableOperation[
            [w, r] =>> Json.Tuple.Schema[S1, w, r],
            [w, r] =>> Json.Tuple.Schema[Json.Or[S1, S2], w, r],
            S2
          ]:
        override def lift[W, R](fa: Json.Tuple.Schema[S1, W, R]): Json.Tuple.Schema[Json.Or[S1, S2], W, R] = fa

        override def element[W, R](fb: => S2[W, R]): Json.Tuple.Schema[Json.Or[S1, S2], W, R] =
          Json.Tuple.Schema.apply[Json.Or[S1, S2], W, R](Self.Tuple.Root(Reference.later(fb)))

  /** A choice between branches: written as whichever branch the value matches, read by trying them in turn.
    *
    * Nothing on the wire says which branch a document belongs to. The name a branch carries labels the path a violation
    * is reported at and is never written out, so a format that discriminates on a `type` field is a union whose
    * branches each hold that field as a [[Json.Constant]], and reading one costs an attempt per branch ahead of it.
    */
  type Union[A] = Json.Union.Of[Json.Node, A]

  object Union:
    /** A union holding `S` and round tripping `A`. `Json.Union[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Node[w, r], A] = Json.Union.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Union.Schema[Json.Node, w, r]

    type Reader[+A] = Json.Union.Reader.Of[Json.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Node[w, r], +A] = Json.Union.Schema[S, Nothing, A]

    type Writer[-A] = Json.Union.Writer.Of[Json.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Node[w, r], -A] = Json.Union.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Union[[w, r] =>> Json.Branch.Schema[S, w, r], W, R]]
    ) extends Json.Schema[S, W, R]

    object Schema
        extends Wrapper.Union[Json.Node, Json.Union.Schema, Json.Branch.Schema](
          [s[-w, +r] <: Json.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Union[[a, b] =>> Json.Branch.Schema[s, a, b], w, r]]) =>
              new Json.Union.Schema(annotation),
          [s[-w, +r] <: Json.Node[w, r], w, r] => (json: Json.Union.Schema[s, w, r]) => json.self
        ):
      given unionable: [S[-w, +r] <: Json.Node[w, r]]
        => UnionableOperation[[w, r] =>> Json.Union.Schema[S, w, r], [w, r] =>> Json.Union.Schema[S, w, r]] =
        UnionableOperation.identity

      /** `union :+ branch`. */
      given alternable: [S1[-w, +r] <: Json.Node[w, r], S2[-w, +r] <: Json.Node[w, r]]
          => AlternableOperation[
            [w, r] =>> Json.Union.Schema[S1, w, r],
            [w, r] =>> Json.Union.Schema[Json.Or[S1, S2], w, r],
            [w, r] =>> Json.Branch.Schema[S2, w, r]
          ]:
        override def lift[W, R](fa: Json.Union.Schema[S1, W, R]): Json.Union.Schema[Json.Or[S1, S2], W, R] = fa

        override def element[W, R](fb: => Json.Branch.Schema[S2, W, R]): Json.Union.Schema[Json.Or[S1, S2], W, R] =
          Json.Union.Schema.apply[Json.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))

  type Primitive[A] = Json.Primitive.Of[Json.Node, A]

  object Primitive:
    /** A primitive holding `S` and round tripping `A`. `Json.Primitive[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Node[w, r], A] = Json.Primitive.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Primitive.Schema[Json.Node, w, r]

    type Reader[+A] = Json.Primitive.Reader.Of[Json.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Node[w, r], +A] = Json.Primitive.Schema[S, Nothing, A]

    type Writer[-A] = Json.Primitive.Writer.Of[Json.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Node[w, r], -A] = Json.Primitive.Schema[S, A, Any]

    sealed abstract class Schema[+S[-w, +r] <: Json.Schema[?, w, r], -W, +R] extends Json.Schema[S, W, R]

    type Boolean[A] = Json.Primitive.Boolean.Schema[A, A]

    object Boolean:
      /** Holding nothing, so `Node` is the schema itself. */
      type Node = [w, r] =>> Json.Primitive.Boolean.Schema[w, r]

      type Reader[+A] = Json.Primitive.Boolean.Schema[Nothing, A]

      type Writer[-A] = Json.Primitive.Boolean.Schema[A, Any]

      final case class Schema[-W, +R](self: Annotation[Self.Primitive.Boolean[W, R]])
          extends Json.Primitive.Schema[Json.Leaf, W, R]

      object Schema
          extends Wrapper.Primitive.Boolean[Json.Primitive.Boolean.Schema](
            [w, r] =>
              (annotation: Annotation[Self.Primitive.Boolean[w, r]]) => new Json.Primitive.Boolean.Schema(annotation),
            [w, r] => (json: Json.Primitive.Boolean.Schema[w, r]) => json.self
          )

    type Number[A] = Json.Primitive.Number.Schema[A, A]

    object Number:
      /** Holding nothing, so `Node` is the schema itself. */
      type Node = [w, r] =>> Json.Primitive.Number.Schema[w, r]

      type Reader[+A] = Json.Primitive.Number.Schema[Nothing, A]

      type Writer[-A] = Json.Primitive.Number.Schema[A, Any]

      final case class Schema[-W, +R](self: Annotation[Self.Primitive.Number[W, R]])
          extends Json.Primitive.Schema[Json.Leaf, W, R]

      object Schema
          extends Wrapper.Primitive.Number[Json.Primitive.Number.Schema](
            [w, r] =>
              (annotation: Annotation[Self.Primitive.Number[w, r]]) => new Json.Primitive.Number.Schema(annotation),
            [w, r] => (json: Json.Primitive.Number.Schema[w, r]) => json.self
          )

    type Text[A] = Json.Primitive.Text.Schema[A, A]

    object Text:
      /** Holding nothing, so `Node` is the schema itself. */
      type Node = [w, r] =>> Json.Primitive.Text.Schema[w, r]

      type Reader[+A] = Json.Primitive.Text.Schema[Nothing, A]

      type Writer[-A] = Json.Primitive.Text.Schema[A, Any]

      final case class Schema[-W, +R](self: Annotation[Self.Primitive.Text[W, R]])
          extends Json.Primitive.Schema[Json.Leaf, W, R]

      object Schema
          extends Wrapper.Primitive.Text[Json.Primitive.Text.Schema](
            [w, r] => (annotation: Annotation[Self.Primitive.Text[w, r]]) => new Json.Primitive.Text.Schema(annotation),
            [w, r] => (json: Json.Primitive.Text.Schema[w, r]) => json.self
          )

    given profunctor: [S[-w, +r] <: Json.Node[w, r]] => Profunctor[[w, r] =>> Json.Primitive.Schema[S, w, r]]:
      override def dimap[W0, R0, W, R](
          self: Json.Primitive.Schema[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Json.Primitive.Schema[S, W, R] = self match
        case self @ Json.Primitive.Boolean.Schema(_) => Json.Primitive.Boolean.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Primitive.Number.Schema(_)  => Json.Primitive.Number.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Primitive.Text.Schema(_)    => Json.Primitive.Text.Schema.profunctor.dimap(self)(f)(g)

    given functor: [S[-w, +r] <: Json.Node[w, r]] => Functor[[a] =>> Json.Primitive.Schema[S, Nothing, a]] =
      Direction.functor[[w, r] =>> Json.Primitive.Schema[S, w, r]]

    given contravariant: [S[-w, +r] <: Json.Node[w, r]] => Contravariant[[a] =>> Json.Primitive.Schema[S, a, Any]] =
      Direction.contravariant[[w, r] =>> Json.Primitive.Schema[S, w, r]]

    given invariant: [S[-w, +r] <: Json.Node[w, r]] => Invariant[[a] =>> Json.Primitive.Schema[S, a, a]] =
      Direction.invariant[[w, r] =>> Json.Primitive.Schema[S, w, r]]

  type Field[A] = Json.Field.Of[Json.Node, A]

  object Field:
    /** A field holding `S` and round tripping `A`. `Json.Field[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Node[w, r], A] = Json.Field.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Field.Schema[Json.Node, w, r]

    type Reader[+A] = Json.Field.Reader.Of[Json.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Node[w, r], +A] = Json.Field.Schema[S, Nothing, A]

    type Writer[-A] = Json.Field.Writer.Of[Json.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Node[w, r], -A] = Json.Field.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Schema[?, w, r], -W, +R](self: Annotation[Self.Field[S, W, R]])

    object Schema
        extends Wrapper.Field[Json.Node, Json.Field.Schema](
          [s[-w, +r] <: Json.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Field[s, w, r]]) => new Json.Field.Schema(annotation),
          [s[-w, +r] <: Json.Node[w, r], w, r] => (json: Json.Field.Schema[s, w, r]) => json.self
        ):
      given recordable: [S[-w, +r] <: Json.Node[w, r]]
        => RecordableOperation[[w, r] =>> Json.Field.Schema[S, w, r], [w, r] =>> Json.Record.Schema[S, w, r]] =
        RecordableOperation.derived

      /** `field :* field`. */
      given appendable: [S1[-w, +r] <: Json.Node[w, r], S2[-w, +r] <: Json.Node[w, r]]
          => AppendableOperation[
            [w, r] =>> Json.Field.Schema[S1, w, r],
            [w, r] =>> Json.Record.Schema[Json.Or[S1, S2], w, r],
            [w, r] =>> Json.Field.Schema[S2, w, r]
          ]:
        override def lift[W, R](fa: Json.Field.Schema[S1, W, R]): Json.Record.Schema[Json.Or[S1, S2], W, R] =
          Json.Record.Schema.apply[Json.Or[S1, S2], W, R](Self.Record.Root(Reference.now(fa)))

        override def element[W, R](fb: => Json.Field.Schema[S2, W, R]): Json.Record.Schema[Json.Or[S1, S2], W, R] =
          Json.Record.Schema.apply[Json.Or[S1, S2], W, R](Self.Record.Root(Reference.later(fb)))

  type Branch[A] = Json.Branch.Of[Json.Node, A]

  object Branch:
    /** A branch holding `S` and round tripping `A`. `Json.Branch[A]` is this with `S` left open. */
    type Of[S[-w, +r] <: Json.Node[w, r], A] = Json.Branch.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Json.Branch.Schema[Json.Node, w, r]

    type Reader[+A] = Json.Branch.Reader.Of[Json.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Json.Node[w, r], +A] = Json.Branch.Schema[S, Nothing, A]

    type Writer[-A] = Json.Branch.Writer.Of[Json.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Json.Node[w, r], -A] = Json.Branch.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Json.Schema[?, w, r], -W, +R](self: Annotation[Self.Branch[S, W, R]])

    object Schema
        extends Wrapper.Branch[Json.Node, Json.Branch.Schema](
          [s[-w, +r] <: Json.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Branch[s, w, r]]) => new Json.Branch.Schema(annotation),
          [s[-w, +r] <: Json.Node[w, r], w, r] => (json: Json.Branch.Schema[s, w, r]) => json.self
        ):
      given unionable: [S[-w, +r] <: Json.Node[w, r]]
        => UnionableOperation[[w, r] =>> Json.Branch.Schema[S, w, r], [w, r] =>> Json.Union.Schema[S, w, r]] =
        UnionableOperation.derived

      /** `branch :+ branch`. */
      given alternable: [S1[-w, +r] <: Json.Node[w, r], S2[-w, +r] <: Json.Node[w, r]]
          => AlternableOperation[
            [w, r] =>> Json.Branch.Schema[S1, w, r],
            [w, r] =>> Json.Union.Schema[Json.Or[S1, S2], w, r],
            [w, r] =>> Json.Branch.Schema[S2, w, r]
          ]:
        override def lift[W, R](fa: Json.Branch.Schema[S1, W, R]): Json.Union.Schema[Json.Or[S1, S2], W, R] =
          Json.Union.Schema.apply[Json.Or[S1, S2], W, R](Self.Union.Root(Reference.now(fa)))

        override def element[W, R](fb: => Json.Branch.Schema[S2, W, R]): Json.Union.Schema[Json.Or[S1, S2], W, R] =
          Json.Union.Schema.apply[Json.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))

  given profunctor: [S[-w, +r] <: Json.Node[w, r]] => Profunctor[[w, r] =>> Json.Schema[S, w, r]]:
    override def dimap[W0, R0, W, R](self: Json.Schema[S, W0, R0])(f: W => W0)(g: R0 => R): Json.Schema[S, W, R] =
      self match
        case self @ Json.Coerce.Schema(_)            => Json.Coerce.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Collection.Schema(_)        => Json.Collection.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Constant.Schema(_)          => Json.Constant.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Dictionary.Schema(_)        => Json.Dictionary.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Enumeration.Schema(_)       => Json.Enumeration.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Optional.Schema(_)          => Json.Optional.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Primitive.Boolean.Schema(_) => Json.Primitive.Boolean.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Primitive.Number.Schema(_)  => Json.Primitive.Number.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Primitive.Text.Schema(_)    => Json.Primitive.Text.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Record.Schema(_)            => Json.Record.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Tuple.Schema(_)             => Json.Tuple.Schema.profunctor.dimap(self)(f)(g)
        case self @ Json.Union.Schema(_)             => Json.Union.Schema.profunctor.dimap(self)(f)(g)

  given functor: [S[-w, +r] <: Json.Node[w, r]] => Functor[[a] =>> Json.Schema[S, Nothing, a]] =
    Direction.functor[[w, r] =>> Json.Schema[S, w, r]]

  given contravariant: [S[-w, +r] <: Json.Node[w, r]] => Contravariant[[a] =>> Json.Schema[S, a, Any]] =
    Direction.contravariant[[w, r] =>> Json.Schema[S, w, r]]

  given invariant: [S[-w, +r] <: Json.Node[w, r]] => Invariant[[a] =>> Json.Schema[S, a, a]] =
    Direction.invariant[[w, r] =>> Json.Schema[S, w, r]]

  /** `S` is bounded to a schema so that these do not also offer `.optional` and `.toTuple` on a field or a branch,
    * which are not schemas and already carry their own `optional`.
    */
  given optionalable: [S[-w, +r] <: Json.Node[w, r]]
    => OptionalableOperation[S, [w, r] =>> Json.Optional.Schema[S, w, r]] = OptionalableOperation.derived

  given tupleable: [S[-w, +r] <: Json.Node[w, r]] => TupleableOperation[S, [w, r] =>> Json.Tuple.Schema[S, w, r]] =
    TupleableOperation.derived
