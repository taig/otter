package io.taig.otter

import cats.data.NonEmptyList

/** What every TypeScript renderer of a JSON schema needs before it can pick a target library. */
object JsonTypescript:
  /** The [[Metadata.Namespace]] the JSON TypeScript renderers read their attributes from, whatever the target library.
    */
  val Namespace: Metadata.Namespace = Metadata.Namespace("json-typescript")

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
    attr(namespaces, metadata(json), Keys.name)

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
  def presence[F[-_, +_], W, R](side: Side, metadata: Metadata, field: Field[F, W, R]): JsonTypescript.Presence =
    field match
      case Field.Root(_, _)         => JsonTypescript.Presence.Required
      case Field.Modify(self, _, _) => presence(side, metadata, self)
      case Field.Default(self, _)   =>
        side match
          case Side.Write => JsonTypescript.Presence.Required
          case Side.Read  => absent(metadata)
      case Field.Optional(self) =>
        side match
          case Side.Write =>
            Json.absence(metadata) match
              case Absence.Omit  => JsonTypescript.Presence.Optional
              case Absence.Empty => JsonTypescript.Presence.Nullable
          case Side.Read => absent(metadata)

  /** Whether a tuple admits the all empty form the codecs use for a tuple that is not there.
    *
    * The same asymmetry a field has, positionally: a tuple holding a default is always written in full and may arrive
    * empty, and one that is optional may be either way round.
    */
  def absent[F[-_, +_], W, R](side: Side, schema: Tuple[F, W, R]): Boolean = schema match
    case Tuple.Empty                => false
    case Tuple.Root(_)              => false
    case Tuple.Product(left, right) => absent(side, left) || absent(side, right)
    case Tuple.Optional(_)          => true
    case Tuple.Modify(self, _, _)   => absent(side, self)
    case Tuple.Default(_, _)        =>
      side match
        case Side.Write => false
        case Side.Read  => true

  /** Which forms of absence a field accepts when read. */
  private def absent(metadata: Metadata): JsonTypescript.Presence = Json.tolerance(metadata) match
    case Tolerance.Lenient => JsonTypescript.Presence.OptionalNullable
    case Tolerance.Strict  =>
      Json.absence(metadata) match
        case Absence.Omit  => JsonTypescript.Presence.Optional
        case Absence.Empty => JsonTypescript.Presence.Nullable
